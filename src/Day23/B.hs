module Day23.B
     ( solve23B
     ) where

import Day23.Common

addCustomTiles :: Int -> [Sideroom] -> [Sideroom] -> [Sideroom]
addCustomTiles after = zipWith (\room2 room1 -> take after room1 ++ room2 ++ drop after room1)

solve23B :: [Sideroom] -> Int
solve23B = getMinResult . start . addCustomTiles 1 [[D, D], [C, B], [B, A], [A, C]]
