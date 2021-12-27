module Day6.A
     ( solve6A
     ) where

import Day6.Common

solve6A :: [Int] -> Int
solve6A = countFish . (!! 80) . iterate tick . listToFishGenerations
