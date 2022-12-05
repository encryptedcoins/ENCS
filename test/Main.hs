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

import           Prelude                          (IO, print)
import           Test.QuickCheck                  (quickCheck)

import           Tests.Distribution

main :: IO ()
main = do
    quickCheck prop_DistributionValidatorParams
    quickCheck prop_DistributionList
    quickCheck prop_DistributionTotal
    print "Testing complete."
