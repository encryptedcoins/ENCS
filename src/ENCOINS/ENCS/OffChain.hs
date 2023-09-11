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

import           Data.Bool                              (bool)
import           Data.Functor                           (($>), void)
import           Ledger.Ada                             (lovelaceValueOf)
import           Ledger.Tx                              (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Value                           (geq, noAdaValue)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                       hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           ENCOINS.ENCS.Types                     (ENCSRedeemer(..))
import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Types.Tx               (TransactionBuilder)

------------------------------------- Distribution Validator --------------------------------------

distributionTx :: DistributionValidatorParamsList -> TransactionBuilder ()
distributionTx [] = failTx "distributionTx" "empty DistributionValidatorParams" Nothing $> ()
distributionTx d@((utxoScript, utxoPubKey) : d') = do
    let val    = txOutValue utxoScript + txOutValue utxoPubKey
        utxos  = Just $ head d
        utxos' = bool (Just $ head d') Nothing $ null d'
        addr   = distributionValidatorAddress utxos
    void $ utxoSpentScriptTx
        (\_ o -> noAdaValue (_decoratedTxOutValue o) `geq` noAdaValue val && _decoratedTxOutAddress o == addr)
        (const . const $ distributionValidatorV utxos) (const . const $ ())
    utxoProducedScriptTx (distributionValidatorHash utxos') Nothing (txOutValue utxoScript) ()
    utxoProducedTx (txOutAddress utxoPubKey) (txOutValue utxoPubKey) (Nothing :: Maybe ())

------------------------------------- ENCS Minting Policy --------------------------------------

encsMintTx :: ENCSParams -> DistributionValidatorParams -> TransactionBuilder ()
encsMintTx par@(ref, amt) distribution = do
    let v = scale amt (encsToken par)
    _ <- utxoSpentPublicKeyTx (\r _ -> ref == r)
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (v + lovelaceValueOf lovelaceInDistributionUTXOs) ()
    tokensMintedTx (encsPolicyV par) Mint v

encsBurnTx :: ENCSParams -> Integer -> TransactionBuilder ()
encsBurnTx par amt = do
    let v = scale amt (encsToken par)
    _ <- utxoSpentPublicKeyTx (\_ o -> _decoratedTxOutValue o `geq` v)
    tokensMintedTx (encsPolicyV par) (Burn amt) (zero-v)