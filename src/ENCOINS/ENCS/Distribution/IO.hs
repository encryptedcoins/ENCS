{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.Distribution.IO where

import           Cardano.Api                      (NetworkId (..), StakeAddress, writeFileJSON)
import qualified Cardano.Api
import           Control.Monad                    (void, zipWithM)
import           Data.Aeson                       (FromJSON, ToJSON, eitherDecodeFileStrict)
import           Data.Function                    (on)
import           Data.List                        (sortBy)
import           Data.Ord                         (Down (..))
import           Data.Tuple                       (swap)
import           GHC.Generics                     (Generic)
import           Ledger.Address                   (Address (..))
import           Prelude                          hiding (Num (..), sum)

import           ENCOINS.ENCS.OnChain             (ENCSParams, encsCurrencySymbol, encsTokenName)
import           PlutusAppsExtra.IO.Blockfrost    (getAddressFromStakeAddress, verifyAsset)
import           PlutusAppsExtra.Utils.Address    (addressToBech32)
import Data.Maybe (fromJust)
import Prelude (sum, (*))

data RawDistribution = RawDistribution
    { addressC :: StakeAddress
    , rewardC  :: Double
    } deriving (Show, Generic, FromJSON, ToJSON)

prepareDistribution :: NetworkId -> FilePath -> FilePath -> IO ()
prepareDistribution networkId from to = do
    lst <- sortBy (compare `on` Down . rewardC) <$> (eitherDecodeFileStrict from >>= either fail pure)
    preparedD <- mapM (\d -> fmap swap . sequence . (ceiling $ (*) 1_000_000 $ rewardC d :: Integer,) . (>>= addressToBech32 networkId) 
        <$> getAddressFromStakeAddress networkId (addressC d)) lst
    let dist = sequence preparedD
    void $ writeFileJSON to dist
    void $ writeFileJSON "dist_sorted.json" lst
    print $ sum $ map snd $ fromJust dist

verifyDistribution :: NetworkId -> ENCSParams -> [(Address, Integer)] -> IO (Either Address [(Address, Integer, Cardano.Api.TxId)])
verifyDistribution networkId par d = do
    txIds <- mapM (\(address, amt) -> verifyAsset networkId (encsCurrencySymbol par) encsTokenName amt address) d
    pure $ zipWithM (\(address, amt) mbTxId -> maybe (Left address) (Right . (address, amt,)) mbTxId) d txIds