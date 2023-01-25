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

import           Data.Functor                                   (($>), void)
import           Ledger.Ada                                     (adaValueOf)
import           Ledger.Tx                                      (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Value                                   (geq, noAdaValue)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           Constraints.OffChain
import           Types.Tx                                       (TransactionBuilder)

------------------------------------- Distribution Validator --------------------------------------

distributionTx :: DistributionValidatorParams -> TransactionBuilder ()
distributionTx [] = failTx "distributionTx" "empty DistributionValidatorParams" Nothing $> ()
distributionTx d@((utxoScript, utxoPubKey) : d') = do
    let val  = txOutValue utxoScript + txOutValue utxoPubKey
        addr = distributionValidatorAddress d
    void $ utxoSpentScriptTx
        (\_ o -> noAdaValue (_decoratedTxOutValue o) `geq` noAdaValue val && _decoratedTxOutAddress o == addr)
        (const . const $ distributionValidatorV d) (const . const $ ())
    utxoProducedScriptTx (distributionValidatorHash d') Nothing (txOutValue utxoScript) (Nothing :: Maybe ())
    utxoProducedTx (txOutAddress utxoPubKey) (txOutValue utxoPubKey) (Nothing :: Maybe ())

------------------------------------- ENCS Minting Policy --------------------------------------

encsMintTx :: ENCSParams -> DistributionValidatorParams -> TransactionBuilder ()
encsMintTx par@(ref, amt) distribution = do
    let v = scale amt (encsToken par)
    _ <- utxoSpentPublicKeyTx (\r _ -> ref == r)
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (v + adaValueOf 2) (Nothing :: Maybe ())
    tokensMintedTx (encsPolicyV par) () v