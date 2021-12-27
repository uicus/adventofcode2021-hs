module Day11.B
     ( solve11B
     ) where

import Utils
import Day11.Common

import Data.List

solve11B :: [[Int]] -> Int
solve11B = fst . head . dropWhile ((<100) . snd) . zip [1..] . collectResult . intoMap
