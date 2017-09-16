{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Profile
    ( Profile(..)
    , CostCentre(..)
    , NodeStats(..)
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as JSObject
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

import IDKeyed
import JSONObject
import Node

data Profile = Profile
  { initial_capabilities :: Int
  , cost_centres         :: [CostCentre]
  , arguments            :: [Text]
  , tick_interval        :: Int
  , profile              :: KeyedNode NodeStats
  , program              :: Text
  , total_alloc          :: Int
  , rts_arguments        :: [Text]
  , end_time             :: Text
  , total_ticks          :: Int
  , total_time           :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

data CostCentre = CostCentre
  { id      :: CostCentreID
  , is_caf  :: Bool
  , label   :: Text
  , module' :: Text
  , src_loc :: Text
  }
instance FromJSON CostCentre where
  parseJSON = JSON.withObject "CostCentre" $ \v ->
    withModule <$> parseJSONObject v <*> v .: "module"
    where
      withModule :: NoModuleCostCentre -> Text -> CostCentre
      withModule NoModuleCostCentre{..} module' = CostCentre{..}
instance ToJSON CostCentre where
  toJSON cc =
    JSON.Object $ JSObject.insert "module" (JSON.String $ module' cc) (toJSONObject $ noModule cc)
    where
      noModule :: CostCentre -> NoModuleCostCentre
      noModule CostCentre{..} = NoModuleCostCentre{..}

data NoModuleCostCentre = NoModuleCostCentre
  { id      :: CostCentreID
  , is_caf  :: Bool
  , label   :: Text
  , src_loc :: Text
  }
  deriving (Generic, FromJSON, ToJSON)
instance FromJSONObject NoModuleCostCentre
instance ToJSONObject NoModuleCostCentre

data NodeStats = NodeStats
  { entries :: Int
  , ticks   :: Int
  , alloc   :: Int
  }
  deriving (Generic, FromJSON, ToJSON)
instance FromJSONObject NodeStats
instance ToJSONObject NodeStats
instance Monoid NodeStats where
  mempty = NodeStats 0 0 0
  mappend (NodeStats x1 y1 z1) (NodeStats x2 y2 z2) = NodeStats (x1 + x2) (y1 + y2) (z1 + z2)
