module Day9.B
     ( solve9B
     ) where

import Day9.Common
import Utils

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

justOrDefault :: (a -> b) -> b -> (Maybe a) -> b
justOrDefault f _ (Just x) = f x
justOrDefault f def Nothing = def

traversable :: Map.Map (Int, Int) Int -> (Int, Int) -> Bool
traversable gridMap coords = justOrDefault (<9) False $ Map.lookup coords gridMap

visitAll :: (Int, Int) -> Set.Set (Int, Int) -> Map.Map (Int, Int) Int -> Set.Set (Int, Int)
visitAll start visited gridMap =
  if Set.member start visited || (not $ traversable gridMap start)
    then visited
    else foldl' (\currentVisited neighbor -> visitAll neighbor currentVisited gridMap) (Set.insert start visited) $ neighborsCoordinatesNonDiagonal gridMap start

basinSize :: Map.Map (Int, Int) Int -> (Int, Int) -> Int
basinSize gridMap start = Set.size $ visitAll start Set.empty gridMap

basinsSizes :: Map.Map (Int, Int) Int -> [Int]
basinsSizes gridMap = map (basinSize gridMap) lowPoints
  where
    lowPoints = map fst . Map.toList $ Map.filterWithKey (const . isLowPoint gridMap) gridMap

solve9B :: [[Int]] -> Int
solve9B grid = x*y*z
  where
    (x:y:z:_) = reverse . sort . basinsSizes . intoMap $ grid
