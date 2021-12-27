module Day15.A
     ( solve15A
     ) where

import Day15.Common
import Utils

import qualified Data.Map.Strict as Map
import qualified Data.Heap as Heap

solve15A :: [[Int]] -> Int
solve15A grid = dijkstra (length grid - 1, (length $ head grid) - 1) (intoMap grid) (Heap.insert (0, [(0, 0)]) Heap.empty) Map.empty
