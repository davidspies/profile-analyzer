{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IDMap
    ( IDMap
    , elems
    , foldCCMap
    , fromList
    , fromOverwriteList
    , intersectionWith
    , mapIDd
    , singleton
    , toList
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import IDKeyed
import JSONObject

newtype IDMap a = IDMap (IntMap a)
  deriving (Functor, Foldable)

instance FromJSONObject a => FromJSON (IDMap a) where
  parseJSON x = fromOverwriteList <$> parseJSON x
instance ToJSONObject a => ToJSON (IDMap a) where
  toJSON x = toJSON $ toList x

instance Monoid a => Monoid (IDMap a) where
  mempty = IDMap IntMap.empty
  mappend (IDMap x) (IDMap y) = IDMap $ IntMap.unionWith mappend x y

fromOverwriteList :: [IDKeyed a] -> IDMap a
fromOverwriteList = IDMap . IntMap.fromList . map toPair

fromList :: Monoid a => [IDKeyed a] -> IDMap a
fromList = IDMap . IntMap.fromListWith mappend . map toPair

toList :: IDMap a -> [IDKeyed a]
toList (IDMap m) = map (uncurry IDKeyed) $ IntMap.toList m

foldCCMap :: Monoid b => (IDKeyed a -> b) -> IDMap a -> b
foldCCMap func = fold . mapIDd func

mapIDd :: (IDKeyed a -> b) -> IDMap a -> IDMap b
mapIDd func (IDMap m) = IDMap $ IntMap.mapWithKey ((func .) . IDKeyed) m

singleton :: IDKeyed a -> IDMap a
singleton (IDKeyed k v) = IDMap $ IntMap.singleton k v

intersectionWith :: (a -> b -> c) -> IDMap a -> IDMap b -> IDMap c
intersectionWith func (IDMap x) (IDMap y) = IDMap $ IntMap.intersectionWith func x y

elems :: IDMap a -> [a]
elems (IDMap x) = IntMap.elems x
