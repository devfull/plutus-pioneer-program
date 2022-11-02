{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE NumericUnderscores             #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE LambdaCase                     #-}

module Week03.Solution1Trace
    ( test
    ) where

import Control.Monad.Freer.Extras qualified
    as Extras                               ( logInfo )

import Data.Aeson qualified
    as A                                    ( Value(..) )
import Data.Default                         ( def )

import System.IO                            ( stdout )

import Ledger                               ( Tx(..) )
import Ledger.Ada                           ( adaSymbol, adaToken )
import Ledger.TimeSlot                      ( slotToBeginPOSIXTime )
import Ledger.Value                         ( Value, valueOf )

import Prelude                              ( IO, String, Show(..) )
import PlutusTx.Prelude

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , activateContractWallet
    , callEndpoint
    , runEmulatorTraceIO'
    , waitNSlots
    , waitUntilSlot
    )

import Plutus.Trace.Emulator.Types
    ( ContractInstanceLog(..)
    , ContractInstanceMsg(..)
    , UserThreadMsg(..)
    )

import Wallet.Emulator.MultiAgent           ( EmulatorEvent'(..) )
import Wallet.Emulator.Wallet               ( WalletEvent(..), knownWallet, mockWalletPaymentPubKeyHash )
import Wallet.Emulator.LogMessages          ( TxBalanceMsg(..) )

import Week03.Solution1                     ( GiveParams(..), endpoints )

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

test :: IO ()
test =
    runEmulatorTraceIO' simpleTraceConfig def myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints

    Extras.logInfo @String $ "Wallet 1 gives, Wallet 2 gives..."
    callEndpoint @"give" h1 (GiveParams (mockWalletPaymentPubKeyHash $ knownWallet 2) (slotToBeginPOSIXTime def 10) 50_000_000)
    callEndpoint @"give" h2 (GiveParams (mockWalletPaymentPubKeyHash $ knownWallet 1) (slotToBeginPOSIXTime def  5) 50_000_000)
    _ <- waitUntilSlot 6

    Extras.logInfo @String $ "Wallet 2 grabs..."
    callEndpoint @"grab" h2 ()
    _ <- waitNSlots 2

    Extras.logInfo @String $ "Identify: Wallet 1 = " <> show (knownWallet 1)
    Extras.logInfo @String $ "Identify: Wallet 2 = " <> show (knownWallet 2)
