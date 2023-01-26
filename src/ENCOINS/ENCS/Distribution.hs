{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.Distribution where

import           Data.Bifunctor                         (Bifunctor (..))
import           Data.Bool                              (bool)
import           Data.Maybe                             (fromJust)
import           Data.Text                              (Text)
import           Ledger.Ada                             (lovelaceValueOf)
import           Plutus.Script.Utils.V2.Scripts         (dataHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Numeric
import           Prelude                                hiding (Num(..))

import           ENCOINS.ENCS.OnChain
import           Utils.Address                          (bech32ToAddress)

type DistributionFee      = Integer
type DistributionFeeCount = Integer
type DistributionParams   = (DistributionFee, DistributionFeeCount)

mkDistribution :: ENCSParams -> [(Address, Integer)] -> DistributionParams -> DistributionValidatorParamsList
mkDistribution _   []                      _ = []
mkDistribution par ((addr, n) : lst) (f, k) =
        (
            TxOut
                (distributionValidatorAddress utxos)
                (scale m (encsToken par) + adaVal)
                (OutputDatumHash $ DatumHash $ dataHash $ toBuiltinData ())
                Nothing,
            TxOut
                addr
                (scale n (encsToken par) + adaVal)
                NoOutputDatum
                Nothing
        ) : distribution
    where
        adaVal       = lovelaceValueOf lovelaceInDistributionUTXOs
        k0           = max (k-1) 0
        distribution = mkDistribution par lst (f, k0)
        utxos        = bool Nothing (Just $ head distribution) $ null distribution
        m            = sum (map snd lst) + f * k0

processDistribution :: [(Text, Integer)] -> [(Address, Integer)]
processDistribution = map (first (fromJust . bech32ToAddress))