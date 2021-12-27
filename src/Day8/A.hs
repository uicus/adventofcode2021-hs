module Day8.A
     ( solve8A
     ) where

solve8A :: [([String], [String])] -> Int
solve8A = length . filter (\x -> elem (length x) [2, 3, 4, 7]) . concat . map snd
