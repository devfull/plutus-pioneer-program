{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Solution1Untyped where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import           Ledger               hiding (singleton, TxInfo(..), ScriptContext(..))
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO)
import qualified Prelude              as P
import           Text.Printf          (printf)

data VestingDatum = VestingDatum
    { beneficiary1 :: PaymentPubKeyHash
    , beneficiary2 :: PaymentPubKeyHash
    , deadline     :: POSIXTime
    } deriving P.Show

PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]

data TxInfo = TxInfo
    { txInfoInputs         :: BuiltinData -- unused field
    , txInfoOutputs        :: BuiltinData -- unused field
    , txInfoFee            :: BuiltinData -- unused field
    , txInfoMint           :: BuiltinData -- unused field
    , txInfoDCert          :: BuiltinData -- unused field
    , txInfoWdrl           :: BuiltinData -- unused field
    , txInfoValidRange     :: POSIXTimeRange
    , txInfoSignatories    :: [PubKeyHash]
    , txInfoData           :: BuiltinData -- unused field
    , txInfoId             :: BuiltinData -- unused field
    }
    deriving stock (Generic, P.Eq)

PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

data CustomScriptContext = CustomScriptContext
    { scriptContextTxInfo  :: TxInfo
    , scriptContextPurpose :: BuiltinData -- unused field
    }
    deriving stock (Generic, P.Eq)

PlutusTx.makeIsDataIndexed ''CustomScriptContext [('CustomScriptContext, 0)]

{-# INLINABLE mkValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> CustomScriptContext -> Bool
mkValidator dat _ ctx
    | (unPaymentPubKeyHash (beneficiary1 dat) `elem` sigs) && (to       (deadline dat) `contains` range) = True
    | (unPaymentPubKeyHash (beneficiary2 dat) `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
    | otherwise                                                                                          = False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigs :: [PubKeyHash]
    sigs = txInfoSignatories info

    range :: POSIXTimeRange
    range = txInfoValidRange info

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped datum _ ctx =
   check $ mkValidator (PlutusTx.unsafeFromBuiltinData datum) () (PlutusTx.unsafeFromBuiltinData ctx)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    pkh <- ownPaymentPubKeyHash
    let dat = VestingDatum
                { beneficiary1 = gpBeneficiary gp
                , beneficiary2 = pkh
                , deadline     = gpDeadline gp
                }
        tx = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData dat) (Ada.lovelaceValueOf $ gpAmount gp)
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (P.show $ gpBeneficiary gp)
        (P.show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now    <- currentTime
    pkh    <- ownPaymentPubKeyHash
    utxos  <- utxosAt scrAddress
    let utxos1 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
        utxos2 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now >  deadline dat) utxos
    logInfo @P.String $ printf "found %d gift(s) to grab" (Map.size utxos1 P.+ Map.size utxos2)
    unless (Map.null utxos1) $ do
        let orefs   = fst <$> Map.toList utxos1
            lookups = Constraints.unspentOutputs utxos1 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (to now)
        void $ submitTxConstraintsWith @Void lookups tx
    unless (Map.null utxos2) $ do
        let orefs   = fst <$> Map.toList utxos2
            lookups = Constraints.unspentOutputs utxos2 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref $ unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (from now)
        void $ submitTxConstraintsWith @Void lookups tx
  where
    isSuitable :: (VestingDatum -> Bool) -> ChainIndexTxOut -> Bool
    isSuitable p o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum d) -> maybe False p $ PlutusTx.fromBuiltinData d

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
