{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Week04.SolutionTrace where

import Control.Monad.Freer.Extras qualified
    as Extras                               ( logInfo )

import Data.Aeson qualified
    as A                                    ( Value(String) )
import Data.Default                         ( def )

import System.IO                            ( stdout )

import Ledger                               ( Tx(..), Value )
import Ledger.Ada                           ( adaSymbol, adaToken )
import Ledger.Value                         ( valueOf )

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , activateContractWallet
    , callEndpoint
    , waitNSlots
    , runEmulatorTraceIO'
    )
import Plutus.Trace.Emulator.Types
    ( ContractInstanceLog(..)
    , ContractInstanceMsg(..)
    , UserThreadMsg(..)
    )

import Wallet.Emulator.Wallet               ( WalletEvent(..), knownWallet, mockWalletPaymentPubKeyHash )
import Wallet.Emulator.MultiAgent           ( EmulatorEvent'(..) )
import Wallet.Emulator.LogMessages          ( TxBalanceMsg(..) )

import Week04.SolutionContract              ( PayParams(..), payContract )


valueOfAda :: Value -> Integer
valueOfAda v = valueOf v adaSymbol adaToken

showFees :: Tx -> String
showFees tx = show (valueOfAda $ txFee tx) <> " lovelaces"

simpleTraceConfig :: TraceConfig
simpleTraceConfig =
    TraceConfig
        { showEvent     = simpleShowEvent
        , outputHandle  = stdout
        }
    where
        simpleShowEvent :: EmulatorEvent' -> Maybe String
        simpleShowEvent = \case
            UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
            InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
            InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> err
            (WalletEvent w (TxBalanceLog (FinishedBalancing tx)))                -> Just $ "*** WALLET EVENT: " <> show w <> " pays " <> showFees tx <> " fees"
            _                                                                    -> Nothing

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h1 <- activateContractWallet (knownWallet 1) payContract

    Extras.logInfo @String $ "Wallet 1 pays Wallet 2..."
    callEndpoint @"pay" h1 $
        PayParams
            { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
            , ppLovelace  = x
            }
    _ <- waitNSlots 1

    Extras.logInfo @String $ "Wallet 1 pays Wallet 2 again..."
    callEndpoint @"pay" h1 $
        PayParams
            { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
            , ppLovelace  = y
            }
    _ <- waitNSlots 1

    Extras.logInfo @String $ "Identify: Wallet 1 = " <> show (knownWallet 1)
    Extras.logInfo @String $ "Identify: Wallet 2 = " <> show (knownWallet 2)

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO' simpleTraceConfig def $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO' simpleTraceConfig def $ payTrace 1000_000_000 20_000_000
