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

import           Ledger.Tokens                   (token)
import           Ledger.Typed.Scripts            (IsScriptContext(..), Versioned (..), Language (..))
import           Ledger.Value                    (AssetClass (..))
import           Plutus.Script.Utils.V2.Address  (mkValidatorAddress)
import           Plutus.Script.Utils.V2.Contexts (spendsOutput)
import           Plutus.Script.Utils.V2.Scripts  (validatorHash, scriptCurrencySymbol)
import           Plutus.V2.Ledger.Api
import           PlutusTx                        (compile, applyCode, liftCode)
import           PlutusTx.Prelude

import           Constraints.OnChain             (utxoProduced, tokensMinted)
import           ENCOINS.ENCS.Types              (ENCSRedeemer(..))

------------------------------------- Distribution Validator --------------------------------------

distributionFee :: Integer
distributionFee = 100

distributionFeeCount :: Integer
distributionFeeCount = 1500

lovelaceInDistributionUTXOs :: Integer
lovelaceInDistributionUTXOs = 1_500_000

type DistributionValidatorParams = [(TxOut, TxOut)]

{-# INLINABLE distributionValidatorCheck #-}
distributionValidatorCheck :: DistributionValidatorParams -> () -> () -> ScriptContext -> Bool
distributionValidatorCheck lst _ _ ScriptContext{scriptContextTxInfo=info} = cond0 && cond1
  where
    (utxo1, utxo2) = head lst

    cond0 = utxoProduced info (== utxo1)
    cond1 = utxoProduced info (== utxo2)

distributionValidator :: DistributionValidatorParams -> Validator
distributionValidator par = mkValidatorScript
    ($$(PlutusTx.compile [|| mkUntypedValidator . distributionValidatorCheck ||])
    `PlutusTx.applyCode` PlutusTx.liftCode par)

distributionValidatorV :: DistributionValidatorParams -> Versioned Validator
distributionValidatorV = flip Versioned PlutusV2 . distributionValidator

distributionValidatorHash :: DistributionValidatorParams -> ValidatorHash
distributionValidatorHash = validatorHash . distributionValidator

distributionValidatorAddress :: DistributionValidatorParams -> Address
distributionValidatorAddress = mkValidatorAddress . distributionValidator

distributionValidatorAddresses :: DistributionValidatorParams -> [Address]
distributionValidatorAddresses []         = []
distributionValidatorAddresses par@(_:ds) = distributionValidatorAddress par : distributionValidatorAddresses ds

------------------------------------- ENCS Minting Policy --------------------------------------

type ENCSParams = (TxOutRef, Integer)

{-# INLINABLE encsTokenName #-}
encsTokenName :: TokenName
encsTokenName = TokenName emptyByteString

encsPolicyCheck :: ENCSParams -> ENCSRedeemer -> ScriptContext -> Bool
encsPolicyCheck (TxOutRef refId refIdx, amt) Mint
    ctx@ScriptContext{scriptContextTxInfo=info} = 
      let cond0 = tokensMinted ctx $ fromList [(encsTokenName, amt)]
          cond1 = spendsOutput info refId refIdx
      in cond0 && cond1
encsPolicyCheck _ (Burn amt) ctx =
      let cond0 = tokensMinted ctx $ fromList [(encsTokenName, -amt)]
          cond1 = amt > 0
      in cond0 && cond1

encsPolicy :: ENCSParams -> MintingPolicy
encsPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . encsPolicyCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

encsPolicyV :: ENCSParams -> Versioned MintingPolicy
encsPolicyV = flip Versioned PlutusV2 . encsPolicy

encsCurrencySymbol :: ENCSParams -> CurrencySymbol
encsCurrencySymbol = scriptCurrencySymbol . encsPolicy

encsAssetClass :: ENCSParams -> AssetClass
encsAssetClass par = AssetClass (encsCurrencySymbol par, encsTokenName)

encsToken :: ENCSParams -> Value
encsToken = token . encsAssetClass