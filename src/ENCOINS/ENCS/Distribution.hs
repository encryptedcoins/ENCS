{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
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
import           Prelude                                hiding (Num (..))

import qualified Cardano.Api
import           Control.Monad                          (zipWithM)
import           ENCOINS.ENCS.OnChain
import           IO.Blockfrost                          (verifyAsset)
import           Utils.Address                          (bech32ToAddress)

type DistributionFee      = Integer
type DistributionFeeCount = Integer
type DistributionParams   = (DistributionFee, DistributionFeeCount)

mkDistribution :: ENCSParams -> [(Address, Integer)] -> DistributionParams -> DistributionValidatorParamsList
mkDistribution _   []                _      = []
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
        utxos        = bool (Just $ head distribution) Nothing $ null distribution
        m            = sum (map snd lst) + f * k0

processDistribution :: [(Text, Integer)] -> [(Address, Integer)]
processDistribution = map (first (fromJust . bech32ToAddress))

verifyDistribution :: ENCSParams -> [(Address, Integer)] -> IO (Either Address [(Address, Integer, Cardano.Api.TxId)])
verifyDistribution par d = do
    txIds <- mapM (\(address, amt) -> verifyAsset (encsPolicyH par) amt address) d
    pure $ zipWithM (\(address, amt) mbTxId -> maybe (Left address) (Right . (address, amt,)) mbTxId) d txIds