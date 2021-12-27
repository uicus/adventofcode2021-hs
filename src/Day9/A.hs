module Day9.A
     ( solve9A
     ) where

import Day9.Common
import Utils

import qualified Data.Map.Strict as Map

solve9A :: [[Int]] -> Int
solve9A grid = Map.foldl' ((+) . (+1)) 0 $ Map.filterWithKey (const . isLowPoint gridMap) gridMap
  where
    gridMap = intoMap grid
