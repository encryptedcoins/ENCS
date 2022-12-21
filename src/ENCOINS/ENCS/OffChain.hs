{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.OffChain where

import           Data.Functor                                   (($>))
import           Ledger                                         (PaymentPubKeyHash (PaymentPubKeyHash), stakingCredential)
import           Ledger.Ada                                     (adaValueOf)
import           Ledger.Tokens                                  (token)
import           Ledger.Value                                   (AssetClass (..))
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           Constraints.OffChain
import           Types.Tx                                       (TransactionBuilder)


------------------------------------- Distribution Validator --------------------------------------

distributionValidator :: DistributionValidatorParams -> Validator
distributionValidator = validatorScript . distributionTypedValidator

distributionValidatorHash :: DistributionValidatorParams -> ValidatorHash
distributionValidatorHash = validatorHash . distributionValidator

distributionValidatorAddress :: DistributionValidatorParams -> Address
distributionValidatorAddress = validatorAddress . distributionTypedValidator

distributionValidatorAddresses :: DistributionValidatorParams -> [Address]
distributionValidatorAddresses []         = []
distributionValidatorAddresses par@(_:ds) = distributionValidatorAddress par : distributionValidatorAddresses ds

distributionTx :: DistributionValidatorParams -> TransactionBuilder ()
distributionTx [] = failTx Nothing $> ()
distributionTx ((utxoScript, utxoPubKey) : distribution) = do
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (txOutValue utxoScript) ()
    let addr = txOutAddress utxoPubKey
    case addr of
        Address (PubKeyCredential pkh) _ ->
            utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) (stakingCredential addr) (txOutValue utxoPubKey) (Nothing :: Maybe ())
        _ -> failTx Nothing $> ()

------------------------------------- ENCS Minting Policy --------------------------------------

encsCurrencySymbol :: ENCSParams -> CurrencySymbol
encsCurrencySymbol = scriptCurrencySymbol . encsPolicy

encsAssetClass :: ENCSParams -> AssetClass
encsAssetClass par = AssetClass (encsCurrencySymbol par, encsTokenName)

encsToken :: ENCSParams -> Value
encsToken = token . encsAssetClass

encsMintTx :: ENCSParams -> DistributionValidatorParams -> TransactionBuilder ()
encsMintTx par@(ref, amt) distribution = do
    let v = scale amt (encsToken par)
    tokensMintedTx (encsPolicy par) () v
    _ <- utxoSpentPublicKeyTx (\r _ -> ref == r)
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (v + adaValueOf 2) ()
        