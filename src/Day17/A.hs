module Day17.A
     ( solve17A
     ) where

import Day17.Common

import Data.List

isPeakReached :: (Int, Int) -> Bool
isPeakReached = (==0) . snd

peak :: Int -> Int
peak initialVelocity = fst $ head $ dropWhile (not . isPeakReached) $ iterate simulateY (0, initialVelocity)

solve17A :: ((Int, Int), (Int, Int)) -> Int
solve17A (_, y) = foldl1' max $ map peak $ filter (flip isYAreaReachable y) [velocityY | velocityY <- filter (>0) $ possibleYs y]
