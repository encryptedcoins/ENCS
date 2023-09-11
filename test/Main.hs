{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Cardano.Api                (NetworkId (..), NetworkMagic(..), FromJSON)
import           Prelude
import           Test.QuickCheck            (quickCheck)

import           Tests.Distribution
import ENCOINS.ENCS.Distribution.IO (prepareDistribution)
import Ledger (TxOutRef(TxOutRef), TxId (TxId), Address)
import Data.Aeson (eitherDecodeFileStrict)
import ENCOINS.ENCS.Distribution (processDistribution, mkDistribution)
import qualified PlutusTx.Prelude as Plutus
import ENCOINS.ENCS.OnChain (distributionValidatorAddresses)
import PlutusAppsExtra.Utils.Address (addressToBech32)

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict

main :: IO ()
main = do
    -- generateTestDistribution "distribution.json" (Testnet $ NetworkMagic 2) 100
    -- prepareDistribution Mainnet "calculator.json" "distribution.json"
    -- quickCheck prop_DistributionValidatorParams
    -- quickCheck prop_DistributionList
    -- quickCheck prop_DistributionTotal
    print "Testing complete."

    par <- decodeOrErrorFromFile "encs-params.json"
    d <- processDistribution <$> decodeOrErrorFromFile "distribution.json" :: IO [(Address, Integer)]
    let fee          = 100_000_000
        nFeeCovered  = Plutus.length d
        lst          = mkDistribution par d (fee, nFeeCovered)
    print $ addressToBech32 Mainnet $ head $ distributionValidatorAddresses lst