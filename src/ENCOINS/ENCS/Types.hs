{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module ENCOINS.ENCS.Types where

import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)
import           PlutusTx                 (unstableMakeIsData)
import           PlutusTx.Prelude         (Integer)
import qualified Prelude                  as Haskell

data ENCSRedeemer = Mint | Burn Integer
  deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''ENCSRedeemer