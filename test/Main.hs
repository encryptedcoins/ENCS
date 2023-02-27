{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Cardano.Api                (NetworkId (..), NetworkMagic(..))
import           Prelude
import           Test.QuickCheck            (quickCheck)

import           Tests.Distribution

main :: IO ()
main = do
    generateTestDistribution "distribution.json" (Testnet $ NetworkMagic 2) 100
    quickCheck prop_DistributionValidatorParams
    quickCheck prop_DistributionList
    quickCheck prop_DistributionTotal
    print "Testing complete."