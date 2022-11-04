{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TypeApplications               #-}

module Week04.SolutionContract where

import Data.Functor (void)
import Data.Aeson                           ( FromJSON, ToJSON )
import Data.Text                            ( Text, unpack )
import GHC.Generics                         ( Generic )
import Ledger                               ( PaymentPubKeyHash )

import Ledger.Ada                           ( lovelaceValueOf )
import Ledger.Constraints                   ( mustPayToPubKey )
import Plutus.Contract
    ( Contract
    , Endpoint
    , awaitPromise
    , endpoint
    , submitTx
    , logInfo
    , handleError
    )


data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    handleError (\err -> logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
    payContract
