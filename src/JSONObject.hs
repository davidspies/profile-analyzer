module JSONObject
    ( FromJSONObject(..)
    , ToJSONObject(..)
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

class FromJSON a => FromJSONObject a where
  parseJSONObject :: JSON.Object -> JSON.Parser a
  parseJSONObject = parseJSON . JSON.Object

class ToJSON a => ToJSONObject a where
  toJSONObject :: a -> JSON.Object
  toJSONObject x = case toJSON x of
    JSON.Object o -> o
    other         -> error $ "Not a JSON Object: " ++ show other
