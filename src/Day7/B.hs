module Day7.B
     ( solve7B
     ) where

import Data.List

neededFuel :: Int -> [Int] -> Int
neededFuel center = foldl' (+) 0 . map (\x -> ((abs $ x-center) * ((abs $ x-center) +1)) `div` 2)

solve7B :: [Int] -> Int
solve7B positions = foldl1' min $ map (flip neededFuel $ positions) [(foldl1' min positions) .. (foldl1' max positions)]
