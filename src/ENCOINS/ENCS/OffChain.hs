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
    let val   = txOutValue utxoScript + txOutValue utxoPubKey
        addrs = distributionValidatorAddresses d
    -- FIX HERE: The next line can cause problems in some cases.
    _ <- utxoSpentScriptTx
        (\_ o -> noAdaValue (_decoratedTxOutValue o) `geq` noAdaValue val && _decoratedTxOutAddress o `elem` addrs)
        (const . const $ distributionValidatorV d') (const . const $ ())
    utxoProducedScriptTx (distributionValidatorHash d') Nothing (txOutValue utxoScript) ()
    let addr = txOutAddress utxoPubKey
    case addr of
        Address (PubKeyCredential pkh) _ ->
            utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) (stakingCredential addr) (txOutValue utxoPubKey) (Nothing :: Maybe ())
        _ -> failTx "distributionTx" "Address doesn't has a PubKeyCredential" Nothing $> ()

------------------------------------- ENCS Minting Policy --------------------------------------

encsMintTx :: ENCSParams -> DistributionValidatorParams -> TransactionBuilder ()
encsMintTx par@(ref, amt) distribution = do
    let v = scale amt (encsToken par)
    _ <- utxoSpentPublicKeyTx (\r _ -> ref == r)
    utxoProducedScriptTx (distributionValidatorHash distribution) Nothing (v + adaValueOf 2) ()
    tokensMintedTx (encsPolicyV par) () v
        