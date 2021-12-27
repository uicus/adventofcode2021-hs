module Day15.B
     ( solve15B
     ) where

import Day15.Common
import Utils

import qualified Data.Map.Strict as Map
import qualified Data.Heap as Heap

adjustRiskLevel :: Int -> Int -> Int
adjustRiskLevel change current = ((current-1+change) `mod` 9)+1

adjustRiskLevelOnSquare :: Int -> [[Int]] -> [[Int]]
adjustRiskLevelOnSquare change = map $ map $ adjustRiskLevel change

multiplyListWithRiskLevel :: Int -> [Int] -> [Int]
multiplyListWithRiskLevel times list = concat $ map (\x -> map (adjustRiskLevel x) list) [0 .. times-1]

multiplyHorizontally :: Int -> [[Int]] -> [[Int]]
multiplyHorizontally times = map (multiplyListWithRiskLevel times)

multiplyVertically :: Int -> [[Int]] -> [[Int]]
multiplyVertically times rows = concat $ map (flip adjustRiskLevelOnSquare rows) [0 .. times-1]

multiplySquares :: Int -> [[Int]] -> [[Int]]
multiplySquares times = multiplyVertically times . multiplyHorizontally times

solve15B :: [[Int]] -> Int
solve15B rawGrid = dijkstra (length grid - 1, (length $ head grid) - 1) (intoMap grid) (Heap.insert (0, [(0, 0)]) Heap.empty) Map.empty
  where
    grid = multiplySquares 5 rawGrid
