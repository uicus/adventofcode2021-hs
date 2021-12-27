module Day1.B
     ( solve1B
     ) where

solve1B :: [Int] -> Int
solve1B input = length $ filter id $ zipWith (<) input (tail.tail.tail $ input)
