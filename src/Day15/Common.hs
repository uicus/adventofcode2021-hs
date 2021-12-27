module Day15.Common
     ( read15
     , dijkstra
     ) where

import Utils

import System.IO
import Control.Monad.Loops
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Heap as Heap

eligibleForResult :: Map.Map (Int, Int) Int -> (Int, Int) -> Int -> Bool
eligibleForResult visited coords distance =
  case Map.lookup coords visited of
    Just x  -> distance < x
    Nothing -> True

dijkstra :: (Int, Int) -> Map.Map (Int, Int) Int -> Heap.MinPrioHeap Int [(Int, Int)] -> Map.Map (Int, Int) Int -> Int
dijkstra end graph heap visited =
  if coords == end
    then distance
    else
      if eligibleForResult visited coords distance
        then dijkstra end graph (foldl' (flip Heap.insert) newHeap $ zip neighborsDistances $ map (:path) neighbors) $ Map.insert coords distance visited
        else dijkstra end graph newHeap visited
  where
    Just ((distance, path@(coords:_)), newHeap) = Heap.view heap
    neighbors = neighborsCoordinatesNonDiagonal graph coords
    neighborsDistances = map ((+distance) . fromJust . flip Map.lookup graph) neighbors
    

read15 :: Handle -> IO [[Int]]
read15 handle = untilM (fmap digits $ hGetLine handle) (hIsEOF handle)
