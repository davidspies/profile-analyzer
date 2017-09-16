{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Char (isNumber)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative
import Prelude hiding (id)
import qualified Prelude as P

import IDKeyed
import IDMap
import JSONObject
import Node
import Profile
import Totalized

data CCStats a = CCStats {cc :: CostCentre, stats :: a}
  deriving (Generic, ToJSON, Functor)
instance ToJSON a => ToJSONObject (CCStats a)

data Structure = List | Tree
  deriving (Enum, Eq, Show, Read)

data Measurement = Ticks | Alloc | Entries
  deriving (Read, Show)

data Phase = Self | Total
  deriving (Read, Show)

data SortKey = SortKey Phase Measurement
  deriving (Read, Show)

data Options = Options
  { path      :: [Text]
  , structure :: Structure
  , sortKey   :: SortKey
  }
  deriving (Read, Show)

data Sorter a = forall b. Ord b => Sorter (a -> b)

sortKeyFunc :: SortKey -> Sorter (Totalized NodeStats)
sortKeyFunc (SortKey phase measurement) = case measurement of
  Ticks   -> Sorter $ \t -> negate $ ticks (withPhase phase t)
  Alloc   -> Sorter $ \t -> negate $ alloc (withPhase phase t)
  Entries -> Sorter $ \t -> negate $ entries (withPhase phase t)
  where
    withPhase = \case
      Self -> self
      Total -> total

profOptions :: ParserInfo Options
profOptions = info (go <**> helper) fullDesc
  where
    go = (\path structure sortKey -> Options{..})
      <$> option auto (long "path")
      <*> option auto (long "structure")
      <*> option auto (long "sort-key")

main :: IO ()
main = do
  opts <- execParser profOptions
  contents <- ByteString.getContents
  Sorter sorter <- return $ sortKeyFunc (sortKey opts)
  let prof = either error P.id $ JSON.eitherDecode contents
      labelToId = Map.fromListWith (++) [(label, [id]) | CostCentre{id, label} <- cost_centres prof]
      pick t = if isNumber (Text.head t) then [read $ Text.unpack t] else labelToId Map.! t
      selected = map pick (path opts) `pathOnly` knMap foldRecursion (profile prof)

      cSing = totals <$> selected
      idToCC = Map.fromList [(id, cc) | cc@CostCentre{id} <- cost_centres prof]
      selectedWithCCs = sortOn (\(IDKeyed _ SortedNode{stats=CCStats{stats}}) -> sorter stats) $
        IDMap.toList $
          sortChildrenOn (\CCStats{stats} -> sorter stats) <$>
          mapIDd (Node.mapWithKey (\(IDKeyed k s) -> CCStats (idToCC Map.! k) s)) cSing

      cDesc = foldCCMap totalCosts selected
      ccs = IDMap.fromOverwriteList [IDKeyed id cc | cc@CostCentre{id} <- cost_centres prof]
      result = IDMap.elems (IDMap.intersectionWith CCStats ccs cDesc)
      sorted = sortOn (\CCStats{stats} -> sorter stats) result
  ByteString.putStrLn $ case structure opts of
    Tree -> encodePretty selectedWithCCs
    List -> encodePretty sorted
