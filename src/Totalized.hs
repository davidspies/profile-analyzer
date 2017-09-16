{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Totalized
    ( Totalized
    , self
    , total
    , totals
    , totalCosts
    , rootedTotalCosts
    ) where

import Data.Aeson (ToJSON(..))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Monoid ((<>))
import GHC.Generics (Generic)

import IDKeyed
import IDMap
import Node

data Totalized a = Totalized
  { self  :: a
  , total :: a
  }
  deriving (Generic, ToJSON)

instance Monoid a => Monoid (Totalized a) where
  mempty = Totalized mempty mempty
  mappend (Totalized x1 y1) (Totalized x2 y2) = Totalized (x1 <> x2) (y1 <> y2)

totals :: Monoid a => Node a -> Node (Totalized a)
totals = rootedTotals IntSet.empty

rootedTotals :: Monoid a => IntSet -> Node a -> Node (Totalized a)
rootedTotals roots = go
  where
    go n = Node
      { stats = Totalized (stats n) (
            stats n <>
            foldCCMap (\(IDKeyed k c) ->
              if k `IntSet.member` roots then mempty else total $ stats c
            ) totChildren
        )
      , children = totChildren
      }
      where
        totChildren = go <$> children n

listDescendants :: KeyedNode a -> [IDKeyed a]
listDescendants = DList.toList . go
  where
    go :: KeyedNode a -> DList (IDKeyed a)
    go (IDKeyed k v) = DList.cons (IDKeyed k (stats v)) $ foldCCMap go $ children v

totalCosts :: forall a. Monoid a => KeyedNode a -> IDMap (Totalized a)
totalCosts = rootedTotalCosts IntSet.empty

rootedTotalCosts :: forall a. Monoid a => IntSet -> KeyedNode a -> IDMap (Totalized a)
rootedTotalCosts roots node =
    IDMap.fromList $ listDescendants $ knMap (clearRedundant IntSet.empty) tots
  where
    clearRedundant :: IntSet -> KeyedNode (Totalized a) -> Node (Totalized a)
    clearRedundant visited (IDKeyed k v) = Node
      { stats = Totalized{self = self $ stats v, total = nextTotals}
      , children = IDMap.mapIDd (clearRedundant nextVisited) $ children v
      }
      where
        (nextTotals, nextVisited)
          | k `IntSet.member` roots   = (total $ stats v, IntSet.empty)
          | k `IntSet.member` visited = (mempty         , visited)
          | otherwise                 = (total $ stats v, IntSet.insert k visited)
    tots = rootedTotals roots <$> node
