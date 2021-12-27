module Day7.A
     ( solve7A
     ) where

import Data.List

solve7A :: [Int] -> Int
solve7A l = foldl' (+) 0 $ map (\x -> abs $ x - center) ordered
  where
    ordered = sort l
    center = ordered !! (length ordered `div` 2)
