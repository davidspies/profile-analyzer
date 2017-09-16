{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IDMap
    ( IDMap
    , delete
    , elems
    , foldCCMap
    , fromList
    , fromOverwriteList
    , insert
    , intersectionWith
    , lookup
    , mapIDd
    , singleton
    , toList
    , traverseMaybeIDd
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Prelude hiding (lookup)

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

insert :: Monoid a => IDKeyed a -> IDMap a -> IDMap a
insert (IDKeyed k v) (IDMap m) = IDMap (IntMap.insertWith mappend k v m)

delete :: CostCentreID -> IDMap a -> IDMap a
delete k (IDMap m) = IDMap (IntMap.delete k m)

-- Not available in containers-0.5.7.1
traverseMaybeWithKey :: Applicative f => (Int -> a -> f (Maybe b)) -> IntMap a -> f (IntMap b)
traverseMaybeWithKey func = fmap (IntMap.mapMaybe id) . IntMap.traverseWithKey func

traverseMaybeIDd :: Applicative f => (IDKeyed a -> f (Maybe b)) -> IDMap a -> f (IDMap b)
traverseMaybeIDd func (IDMap m) = IDMap <$> traverseMaybeWithKey ((func .) . IDKeyed) m

lookup :: CostCentreID -> IDMap a -> Maybe a
lookup k (IDMap m) = IntMap.lookup k m
