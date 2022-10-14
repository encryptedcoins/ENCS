{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module ENCOINS.ENCS.OnChain where

import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator, mkTypedValidator, mkUntypedValidator)
import           Plutus.V2.Ledger.Api
import           PlutusTx                             (compile, applyCode, liftCode)
import           PlutusTx.Prelude

import           Scripts.Constraints                  (utxoProduced)
import           Scripts.OneShotCurrency              (mkCurrency, oneShotCurrencyPolicy)

------------------------------------- Distribution Validator --------------------------------------

distributionFee :: Integer
distributionFee = 100

adaInDistributionUTXOs :: Integer
adaInDistributionUTXOs = 1_000_000

type DistributionValidatorParams = [(TxOut, TxOut)]

data Distributing
instance ValidatorTypes Distributing where
  type instance DatumType Distributing = ()
  type instance RedeemerType Distributing = ()

{-# INLINABLE distributionValidatorCheck #-}
distributionValidatorCheck :: DistributionValidatorParams -> () -> () -> ScriptContext -> Bool
distributionValidatorCheck lst _ _ ScriptContext{scriptContextTxInfo=info} = cond0 && cond1
  where
    (utxo1, utxo2) = head lst

    cond0 = utxoProduced info (== utxo1)
    cond1 = utxoProduced info (== utxo2)

distributionTypedValidator :: DistributionValidatorParams -> TypedValidator Distributing
distributionTypedValidator par = mkTypedValidator @Distributing
    ($$(PlutusTx.compile [|| distributionValidatorCheck ||])
    `PlutusTx.applyCode` PlutusTx.liftCode par)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()

------------------------------------- ENCS Minting Policy --------------------------------------

encsTotalCount :: Integer
encsTotalCount = 15_000_000

type ENCSParams = TxOutRef

{-# INLINABLE encsTokenName #-}
encsTokenName :: TokenName
encsTokenName = TokenName emptyByteString

encsPolicy :: ENCSParams -> MintingPolicy
encsPolicy ref = oneShotCurrencyPolicy $ mkCurrency ref [(encsTokenName, encsTotalCount)]