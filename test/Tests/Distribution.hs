module Tests.Distribution where

import           Cardano.Api                      (NetworkId (..), writeFileJSON)
import           Control.Monad                    (void)
import           Data.Maybe                       (mapMaybe)
import           Ledger.Ada                       (lovelaceValueOf)
import           Ledger.Address                   (Address (..))
import           Ledger.Value                     (geq, noAdaValue)
import           Plutus.V2.Ledger.Api             (Credential (..), OutputDatum (..), PubKeyHash (..), StakingCredential (..), TxOut (..))
import           PlutusTx.Numeric
import           PlutusTx.Prelude                 (modulo, sha2_256, sum, takeByteString)
import           Prelude                          hiding (Num (..), sum)
import qualified Prelude                          as Haskell
import           System.Random                    (randomIO, randomRIO)
import           Test.QuickCheck                  (Arbitrary (..))

import           ENCOINS.ENCS.Distribution        (DistributionParams, mkDistribution)
import           ENCOINS.ENCS.OnChain
import           PlutusTx.Extra.ByteString        (toBytes)
import           PlutusAppsExtra.Utils.Address    (addressToBech32)
import           PlutusAppsExtra.Utils.Orphans    ()

data TestArgs = TestArgs
    {
        testArgsENCSParams  :: ENCSParams,
        testArgsAddressList :: [(Address, Integer)],
        testArgsDistParams  :: DistributionParams,
        testArgsDVP         :: DistributionValidatorParamsList
    }
    deriving (Haskell.Show, Haskell.Eq)

instance Arbitrary TestArgs where
    arbitrary = do
        -- reference to mint ENCS
        ref <- arbitrary

        n <- (`modulo` 10000) <$> arbitrary
        pkhs <- map PubKeyCredential <$> mapM (const arbitrary) [1..n]
        skhs <- map (fmap (StakingHash . PubKeyCredential)) <$>  mapM (const arbitrary) [1..n]
        let addrs = zipWith Address pkhs skhs
        -- distribution list
        lst <- zip addrs . map (max (1 :: Integer)) <$> arbitrary

        -- Total undistributed ENCS
        v   <- max 0 <$> arbitrary

        -- DistributionParams
        distFee      <- abs <$> arbitrary
        distFeeCount <- abs <$> arbitrary
        let distParams = (distFee, distFeeCount)

        -- Total ENCS to distribute
        let amtD = sum (map snd lst)
        -- Total distribution fees
            amtF = distFee * distFeeCount
        -- ENCSParams
            amt = amtD + amtF + v
            par = (ref, amt)

        let dvp = mkDistribution par lst distParams

        return $ TestArgs par lst distParams dvp

----------------------------------------------------------------------------------------------------------------------------

-- Check that at every step we have enough tokens locked.
checkValidatorLock :: ENCSParams -> DistributionValidatorParamsList -> Bool
checkValidatorLock _ [] = True
checkValidatorLock par ((o, _) : d') = cond && checkValidatorLock par d'
    where
        val  = txOutValue o
        d''  = map snd d'
        cond = noAdaValue val `geq` noAdaValue (sum $ map txOutValue d'')

prop_DistributionValidatorParams :: TestArgs -> Bool
prop_DistributionValidatorParams (TestArgs par _ _ dvp) = checkValidatorLock par dvp

----------------------------------------------------------------------------------------------------------------------------

-- Check that every recipient gets their tokens.
checkDistributionList :: ENCSParams -> [(Address, Integer)] -> DistributionValidatorParamsList -> Bool
checkDistributionList par lst d = all f lst
    where
        adaVal      = lovelaceValueOf lovelaceInDistributionUTXOs
        d'          = map snd d
        f (addr, m) = any (\o -> o == TxOut addr (scale m (encsToken par) + adaVal) NoOutputDatum Nothing) d'

prop_DistributionList :: TestArgs -> Bool
prop_DistributionList (TestArgs par lst _ dvp) = checkDistributionList par lst dvp

----------------------------------------------------------------------------------------------------------------------------

-- Check that the total ENCS token number is enough to do the distribution.
checkDistributionTotal :: ENCSParams -> DistributionValidatorParamsList -> Bool
checkDistributionTotal par@(_, amt) d = scale amt (encsToken par) `geq` noAdaValue (sum (map (txOutValue . snd) d))

prop_DistributionTotal :: TestArgs -> Bool
prop_DistributionTotal (TestArgs par _ _ dvp) = checkDistributionTotal par dvp

---------------------------------------------------- Utility functions ----------------------------------------------------

generateTestDistribution :: FilePath -> NetworkId -> Integer -> IO ()
generateTestDistribution file networkId n = do
    pkhs <- map (PubKeyCredential . PubKeyHash . takeByteString 28 . sha2_256 . toBytes) <$> mapM (const (randomIO :: IO Integer)) [1..n]
    skhs <- map (Just . StakingHash . PubKeyCredential . PubKeyHash . takeByteString 28 . sha2_256 . toBytes) <$>
        mapM (const (randomIO :: IO Integer)) [1..n]
    let addrs = mapMaybe (addressToBech32 networkId) (zipWith Address pkhs skhs)
    -- distribution list
    lst <- zip addrs <$> mapM (const $ randomRIO (1::Integer, 10000)) [1::Integer ..n]

    void $ writeFileJSON file lst