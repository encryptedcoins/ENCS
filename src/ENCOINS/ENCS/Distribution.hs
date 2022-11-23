{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.Distribution where

import           Data.Maybe                                     (fromJust)
import           Data.Text                                      (Text)
import           Ledger.Ada                                     (lovelaceValueOf)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           ENCOINS.ENCS.OffChain                          (encsToken, distributionValidatorAddress)
import           Utils.Address                                  (bech32ToAddress)


mkDistribution :: ENCSParams -> [(Text, Integer)] -> Integer -> DistributionValidatorParams
mkDistribution _   []                      _ = []
mkDistribution par ((addrBech32, n) : lst) k =
        (TxOut (distributionValidatorAddress distribution) (scale m (encsToken par) + adaVal) NoOutputDatum Nothing,
        TxOut addr (scale n (encsToken par) + adaVal) NoOutputDatum Nothing) : distribution
    where
        adaVal       = lovelaceValueOf adaInDistributionUTXOs
        distribution = mkDistribution par lst (k-1)
        m            = sum (map snd lst) + distributionFee * (k-1)
        addr         = fromJust $ bech32ToAddress addrBech32

checkDistribution :: ENCSParams -> DistributionValidatorParams -> Value -> Bool
checkDistribution _   []                    vTotal = zero == vTotal
checkDistribution par ((utxo1, utxo2) : d)  vTotal = v    == vTotal && checkDistribution par d (vTotal - v)
    where v = txOutValue utxo1 + txOutValue utxo2 + scale distributionFee (encsToken par)