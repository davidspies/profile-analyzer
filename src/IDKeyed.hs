{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module IDKeyed
    ( CostCentreID
    , IDKeyed(..)
    , knMap
    , toPair
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as JSObject

import JSONObject (FromJSONObject, ToJSONObject, parseJSONObject, toJSONObject)

type CostCentreID = Int

data IDKeyed a = IDKeyed CostCentreID a
  deriving (Functor)
toPair :: IDKeyed a -> (CostCentreID, a)
toPair (IDKeyed k v) = (k, v)
knMap :: (IDKeyed a -> b) -> IDKeyed a -> IDKeyed b
knMap func v@(IDKeyed k _) = IDKeyed k $ func v

instance FromJSONObject a => FromJSON (IDKeyed a) where
  parseJSON = JSON.withObject "IDKeyed" $ \v ->
    IDKeyed <$> v .: "id" <*> parseJSONObject v
instance ToJSONObject a => ToJSON (IDKeyed a) where
  toJSON (IDKeyed ccid n) =
    JSON.Object $ JSObject.insert "id" (JSON.Number $ fromIntegral ccid) $ toJSONObject n
