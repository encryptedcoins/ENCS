{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.Distribution where

import           Data.Bifunctor                                 (Bifunctor (..))
import           Data.Maybe                                     (fromJust)
import           Data.Text                                      (Text)
import           Ledger.Ada                                     (lovelaceValueOf)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.ENCS.OnChain
import           Utils.Address                                  (bech32ToAddress)

mkDistribution :: ENCSParams -> [(Address, Integer)] -> Integer -> DistributionValidatorParams
mkDistribution _   []                      _ = []
mkDistribution par ((addr, n) : lst) k =
        (TxOut (distributionValidatorAddress distribution) (scale m (encsToken par) + adaVal) NoOutputDatum Nothing,
        TxOut addr (scale n (encsToken par) + adaVal) NoOutputDatum Nothing) : distribution
    where
        adaVal       = lovelaceValueOf lovelaceInDistributionUTXOs
        k0           = max (k-1) 0
        distribution = mkDistribution par lst k0
        m            = sum (map snd lst) + distributionFee * k0

processDistribution :: [(Text, Integer)] -> [(Address, Integer)]
processDistribution = map (first (fromJust . bech32ToAddress))