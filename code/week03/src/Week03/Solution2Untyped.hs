{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.Solution2Untyped where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton, TxInfo(..), ScriptContext(..))
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Eq, Semigroup (..), Show (..), String, undefined)
import           Text.Printf          (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    deriving stock (Generic, Prelude.Eq)

PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

data CustomScriptContext = CustomScriptContext
    { scriptContextTxInfo  :: TxInfo
    , scriptContextPurpose :: BuiltinData -- unused field
    }
    deriving stock (Generic, Prelude.Eq)

PlutusTx.makeIsDataIndexed ''CustomScriptContext [('CustomScriptContext, 0)]

{-# INLINABLE mkValidator #-}
mkValidator :: PaymentPubKeyHash -> POSIXTime -> () -> CustomScriptContext -> Bool
mkValidator pkh s () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from s `contains` txInfoValidRange info

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped param datum _ ctx =
   check $ mkValidator (PlutusTx.unsafeFromBuiltinData param) (PlutusTx.unsafeFromBuiltinData datum) () (PlutusTx.unsafeFromBuiltinData ctx)

validator :: BuiltinData -> Validator
validator p = mkValidatorScript ($$(PlutusTx.compile [|| mkValidatorUntyped ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)

valHash :: BuiltinData -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . validator

scrAddress :: BuiltinData -> Ledger.Address
scrAddress = scriptAddress . validator

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
    let p  = gpBeneficiary gp
        d  = gpDeadline gp
        tx = Constraints.mustPayToOtherScript (valHash $ PlutusTx.toBuiltinData p) (Datum $ PlutusTx.toBuiltinData d) (Ada.lovelaceValueOf $ gpAmount gp)
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable now) <$> utxosAt (scrAddress $ PlutusTx.toBuiltinData pkh)
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos        <>
                          Constraints.otherScript (validator $ PlutusTx.toBuiltinData pkh)
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
