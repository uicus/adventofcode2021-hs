module Day6.B
     ( solve6B
     ) where

import Day6.Common

solve6B :: [Int] -> Int
solve6B = countFish . (!! 256) . iterate tick . listToFishGenerations
