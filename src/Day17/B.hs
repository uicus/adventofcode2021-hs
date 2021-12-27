module Day17.B
     ( solve17B
     ) where

import Day17.Common

solve17B :: ((Int, Int), (Int, Int)) -> Int
solve17B area@(x, y) = length $ filter (flip isAreaReachable area) [(velocityX, velocityY) | velocityX <- possibleXs x, velocityY <- possibleYs y]
