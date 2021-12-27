module Day1.A
     ( solve1A
     ) where

solve1A :: [Int] -> Int
solve1A input = length $ filter id $ zipWith (<) input (tail input)
