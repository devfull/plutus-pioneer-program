{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE NumericUnderscores             #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE LambdaCase                     #-}

module Week01.EnglishAuctionTrace
    ( test
    ) where

import Control.Monad.Freer.Extras qualified
    as Extras                               ( logInfo )

import Data.Aeson qualified
    as A                                    ( Value(String) )
import Data.Default                         ( def )
import Data.Map                             ( update )

import System.IO                            ( stdout )

import Ledger.TimeSlot                      ( slotToBeginPOSIXTime )
import Ledger.Value                         ( CurrencySymbol(..), TokenName(..), singleton )

import Prelude                              ( IO, String, Show(..) )
import PlutusTx.Prelude

import Plutus.Contract.Trace                ( defaultDist, knownWallet )
import Plutus.Trace
    ( EmulatorConfig(..)
    , EmulatorTrace
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

import Week01.EnglishAuction
    ( StartParams(..)
    , BidParams(..)
    , CloseParams(..)
    , endpoints
    )


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
            _                                                                    -> Nothing

test :: IO ()
test =
    let nft = singleton (CurrencySymbol "f") (TokenName "T") 1
        withNFT = update (\v -> Just (v <> nft))
        emulatorConfig = EmulatorConfig
            { _initialChainState = Left $ withNFT (knownWallet 1) defaultDist
            , _slotConfig = def
            , _feeConfig = def
            }
    in
        runEmulatorTraceIO' simpleTraceConfig emulatorConfig myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints

    Extras.logInfo @String $ "Starting auction..."
    Extras.logInfo @String $ "Deadline = " <> show (slotToBeginPOSIXTime def 10)
    callEndpoint @"start" h1 $ StartParams
        { spDeadline = slotToBeginPOSIXTime def 10
        , spMinBid   = 10_000_000
        , spCurrency = CurrencySymbol "f"
        , spToken    = TokenName "T"
        }
    _ <- waitNSlots 1

    Extras.logInfo @String $ "Wallet 2 bids..."
    callEndpoint @"bid" h2 $ BidParams
        { bpCurrency = CurrencySymbol "f"
        , bpToken    = TokenName "T"
        , bpBid      = 10_000_000
        }
    _ <- waitNSlots 1

    Extras.logInfo @String $ "Wallet 3 bids..."
    callEndpoint @"bid" h3 $ BidParams
        { bpCurrency = CurrencySymbol "f"
        , bpToken    = TokenName "T"
        , bpBid      = 15_000_000
        }
    _ <- waitUntilSlot 11

    Extras.logInfo @String $ "Closing auction..."
    callEndpoint @"close" h1 $ CloseParams
        { cpCurrency = CurrencySymbol "f"
        , cpToken    = TokenName "T"
        }
    _ <- waitNSlots 1

    Extras.logInfo @String $ "Identify: Wallet 1 = " <> show (knownWallet 1)
    Extras.logInfo @String $ "Identify: Wallet 2 = " <> show (knownWallet 2)
    Extras.logInfo @String $ "Identify: Wallet 3 = " <> show (knownWallet 3)
