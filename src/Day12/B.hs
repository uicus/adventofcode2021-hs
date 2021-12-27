module Day12.B
     ( solve12B
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

numberOfPaths :: Map.Map Int [Int] -> Set.Set Int -> Set.Set Int -> Set.Set Int -> Int -> Int -> Int
numberOfPaths graph big visited visitedTwice start end =
  if start == end
    then 1
    else
      if Set.member start visited
        then
          if Set.member start visitedTwice || Set.size visitedTwice >= 3
            then 0
            else foldl' (+) 0 $ map (\neighbor -> numberOfPaths graph big visited (Set.insert start visitedTwice) neighbor end) $ fromJust $ Map.lookup start graph
        else foldl' (+) 0 $ map (\neighbor -> numberOfPaths graph big (visitIfSmall big visited start) visitedTwice neighbor end) $ fromJust $ Map.lookup start graph

solve12B :: (Map.Map Int [Int], Set.Set Int, Int, Int) -> Int
solve12B (graph, big, start, end) = numberOfPaths graph big (Set.empty) (Set.insert end $ Set.insert start $ Set.empty) start end
