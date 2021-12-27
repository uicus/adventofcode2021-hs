module Day3.B
     ( solve3B
     ) where

import Day3.Common
import Utils

import Data.List

countHeads :: [[Bool]] -> Result
countHeads = foldl' applyBit Result { falses = 0, trues = 0 } . map head

getEligible :: (Result -> Bool) -> [[Bool]] -> [Bool]
getEligible _ [x] = x
getEligible resToBool pool = nextBit:(getEligible resToBool $ map tail $ filter ((==nextBit) . head) pool)
  where
    nextBit = resToBool $ countHeads pool

getOxygenRating :: [[Bool]] -> Int
getOxygenRating = boolsToInt . getEligible chooseLargest

getCo2Rating :: [[Bool]] -> Int
getCo2Rating = boolsToInt . getEligible chooseLowest

solve3B :: [[Bool]] -> Int
solve3B rows = getOxygenRating rows * getCo2Rating rows
