module Day3.A
     ( solve3A
     ) where

import Day3.Common
import Utils

import Data.List

applyRow :: [Result] -> [Bool] -> [Result]
applyRow result = forceElements result `seq` zipWith applyBit result

startResults :: Int -> [Result]
startResults = flip take (repeat Result { falses = 0, trues = 0 })

countOccurences :: [[Bool]] -> [Result]
countOccurences rows = foldl' applyRow (startResults . length . head $ rows) rows

gamma :: [Result] -> Int
gamma = boolsToInt . (map chooseLargest)

epsilon :: [Result] -> Int
epsilon = boolsToInt . (map chooseLowest)

solve3A :: [[Bool]] -> Int
solve3A rows = gamma finalResult * epsilon finalResult
  where
    finalResult = countOccurences rows
