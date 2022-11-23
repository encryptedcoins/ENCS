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

import           Control.Monad.State                            (State)
import           Data.Functor                                   (($>))
import           Ledger                                         (PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash (StakePubKeyHash))
import           Ledger.Tokens                                  (token)
import           Ledger.Typed.Scripts                           (Any)
import           Ledger.Value                                   (AssetClass (..))
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (ValidatorTypes (..), validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           Scripts.Constraints
import           Types.TxConstructor                            (TxConstructor (..))


type EncoinsTransaction = TxConstructor () Any (RedeemerType Any) (DatumType Any)
type EncoinsTransactionBuilder a = State EncoinsTransaction a

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

distributionTx :: DistributionValidatorParams -> EncoinsTransactionBuilder ()
distributionTx [] = failTx Nothing $> ()
distributionTx ((utxoScript, utxoPubKey) : distribution) = do
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (txOutValue utxoScript) ()
    let addr = txOutAddress utxoPubKey
    case addr of
        Address (PubKeyCredential pkh) (Just (StakingHash (PubKeyCredential skh))) ->
            utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) (Just $ StakePubKeyHash skh) (txOutValue utxoPubKey) (Nothing :: Maybe ())
        _ -> failTx Nothing $> ()

------------------------------------- ENCS Minting Policy --------------------------------------

encsCurrencySymbol :: ENCSParams -> CurrencySymbol
encsCurrencySymbol = scriptCurrencySymbol . encsPolicy

encsAssetClass :: ENCSParams -> AssetClass
encsAssetClass par = AssetClass (encsCurrencySymbol par, encsTokenName)

encsToken :: ENCSParams -> Value
encsToken = token . encsAssetClass

encsMintTx :: ENCSParams -> DistributionValidatorParams -> EncoinsTransactionBuilder ()
encsMintTx par distribution = do
    let v = scale encsTotalCount (encsToken par)
    tokensMintedTx (encsPolicy par) () v
    _ <- utxoSpentPublicKeyTx (\r _ -> par == r)
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing v ()
        