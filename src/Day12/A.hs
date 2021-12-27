module Day12.A
     ( solve12A
     ) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

visitIfSmall :: Set.Set Int -> Set.Set Int -> Int -> Set.Set Int
visitIfSmall big visited current =
  if Set.member current big
    then visited
    else Set.insert current visited

numberOfPaths :: Map.Map Int [Int] -> Set.Set Int -> Set.Set Int -> Int -> Int -> Int
numberOfPaths graph big visited start end =
  if start == end
    then 1
    else
      if Set.member start visited
        then 0
        else foldl' (+) 0 $ map (\neighbor -> numberOfPaths graph big (visitIfSmall big visited start) neighbor end) $ fromJust $ Map.lookup start graph

solve12A :: (Map.Map Int [Int], Set.Set Int, Int, Int) -> Int
solve12A (graph, big, start, end) = numberOfPaths graph big (Set.empty) start end
