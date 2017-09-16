{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Totalized
    ( Totalized
    , self
    , total
    , totals
    , totalCosts
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
import Node (KeyedNode, Node(..))

data Totalized a = Totalized
  { self  :: a
  , total :: a
  }
  deriving (Generic, ToJSON)

instance Monoid a => Monoid (Totalized a) where
  mempty = Totalized mempty mempty
  mappend (Totalized x1 y1) (Totalized x2 y2) = Totalized (x1 <> x2) (y1 <> y2)

totals :: Monoid a => Node a -> Node (Totalized a)
totals n = Node
  { stats = Totalized (stats n) (stats n <> foldMap (total . stats) totChildren)
  , children = totChildren
  }
  where
    totChildren = totals <$> children n

listDescendants :: KeyedNode a -> [IDKeyed a]
listDescendants = DList.toList . go
  where
    go :: KeyedNode a -> DList (IDKeyed a)
    go (IDKeyed k v) = DList.cons (IDKeyed k (stats v)) $ foldCCMap go $ children v

totalCosts :: forall a. Monoid a => KeyedNode a -> IDMap (Totalized a)
totalCosts node = IDMap.fromList $ listDescendants $ knMap (clearRedundant IntSet.empty) tots
  where
    clearRedundant :: IntSet -> KeyedNode (Totalized a) -> Node (Totalized a)
    clearRedundant visited (IDKeyed k v)
      | k `IntSet.member` visited = Node
          { stats = Totalized (self $ stats v) mempty
          , children = IDMap.mapIDd (clearRedundant visited) $ children v
          }
      | otherwise = Node
          { stats = stats v
          , children = IDMap.mapIDd (clearRedundant (IntSet.insert k visited)) $ children v
          }
    tots = totals <$> node
