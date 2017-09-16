{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Node
    ( Node(..)
    , KeyedNode
    , SortedNode
    , mapWithKey
    , pathOnly
    , sortChildrenOn
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as JSObject
import Data.List (sortOn)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import JSONObject (FromJSONObject, ToJSONObject, parseJSONObject, toJSONObject)

import IDKeyed
import IDMap

data Node a = Node
  { stats    :: a
  , children :: IDMap (Node a)
  }
  deriving (Functor)
instance FromJSONObject a => FromJSON (Node a) where
  parseJSON = JSON.withObject "Node" parseJSONObject
instance FromJSONObject a => FromJSONObject (Node a) where
  parseJSONObject v = Node <$> parseJSONObject v <*> v .: "children"
instance ToJSONObject a => ToJSON (Node a) where
  toJSON = JSON.Object . toJSONObject
instance ToJSONObject a => ToJSONObject (Node a) where
  toJSONObject Node{stats, children} =
    JSObject.insert "children" (toJSON children) $ toJSONObject stats
instance Monoid a => Monoid (Node a) where
  mempty = Node mempty mempty
  mappend (Node s1 c1) (Node s2 c2) = Node (s1 <> s2) (c1 <> c2)

type KeyedNode a = IDKeyed (Node a)

mapWithKey :: (IDKeyed a -> b) -> KeyedNode a -> Node b
mapWithKey func (IDKeyed k Node{..}) = Node
  { stats = func (IDKeyed k stats)
  , children = IDMap.mapIDd (mapWithKey func) children
  }

pathOnly :: Monoid a => [[CostCentreID]] -> KeyedNode a -> IDMap (Node a)
pathOnly = \case
  [] -> IDMap.singleton
  is@(i : ir) -> \(IDKeyed c Node{children}) ->
    let nextIs = if c `elem` i then ir else is in
    foldCCMap (pathOnly nextIs) children

data SortedNode a = SortedNode
  { stats    :: a
  , children :: [SortedNode a]
  }
  deriving (Generic, ToJSON)

instance ToJSON a => ToJSONObject (SortedNode a)

sortChildrenOn :: Ord b => (a -> b) -> Node a -> SortedNode a
sortChildrenOn cmprtr = go
  where
    go Node{stats = upstats, children} = SortedNode
      { stats = upstats
      , children = sortOn (\SortedNode{stats} -> cmprtr stats) $ go <$> IDMap.elems children
      }
