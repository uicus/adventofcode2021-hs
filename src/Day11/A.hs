module Day11.A
     ( solve11A
     ) where

import Utils
import Day11.Common

import Data.List

solve11A :: [[Int]] -> Int
solve11A = foldl' (+) 0 . take 100 . collectResult . intoMap
