{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.ENCS.Distribution.IO where

import           Cardano.Api                   (NetworkId (..), StakeAddress, TxId, writeFileJSON)
import           Control.Monad                 (void)
import           Data.Aeson                    (FromJSON, ToJSON, eitherDecodeFileStrict)
import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Ord                      (Down (..))
import           Data.Tuple                    (swap)
import           ENCOINS.ENCS.OnChain          (ENCSParams, encsCurrencySymbol, encsTokenName)
import           GHC.Generics                  (Generic)
import           Ledger.Address                (Address (..))
import           PlutusAppsExtra.IO.Blockfrost (getAddressFromStakeAddress, verifyAssetFast)
import           PlutusAppsExtra.Utils.Address (addressToBech32)
import           Prelude                       hiding (Num (..), sum)

data RawDistribution = RawDistribution
    { addressC :: StakeAddress
    , rewardC  :: Double
    } deriving (Show, Generic, FromJSON, ToJSON)

prepareDistribution :: NetworkId -> FilePath -> FilePath -> IO ()
prepareDistribution networkId from to = do
    lst <- sortBy (compare `on` Down . rewardC) <$> (eitherDecodeFileStrict from >>= either fail pure)
    preparedD <- mapM (\d -> fmap swap . sequence . (rewardC d,) . (>>= addressToBech32 networkId) 
        <$> getAddressFromStakeAddress networkId (addressC d)) lst
    void $ writeFileJSON to $ sequence preparedD

verifyDistribution 
    :: NetworkId
    -> ENCSParams 
    -> [(Address, Integer)]
    -> Maybe ([(Address, Integer, TxId)] -> IO ()) -- Function to save intermidiate results
    -> [(Address, Integer, Cardano.Api.TxId)]      -- Already verified addresses
    -> IO [Either Address (Address, Integer, TxId)]
verifyDistribution network par = verifyAssetFast network (encsCurrencySymbol par) encsTokenName