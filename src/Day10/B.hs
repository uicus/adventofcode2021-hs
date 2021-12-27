module Day10.B
     ( solve10B
     ) where

import Day10.Common

import Data.List
import Data.Maybe
import Data.Either.Combinators

closingSequence :: String -> Maybe [Char]
closingSequence = fmap (map correspondingBracket) . rightToMaybe . parse []

score :: Char -> Int
score ')' = 1
score '}' = 3
score ']' = 2
score '>' = 4

mean :: Ord a => [a] -> a
mean input = ordered !! (length ordered `div` 2)
  where
    ordered = sort input

solve10B :: [String] -> Int
solve10B = mean . map (foldl' (\x d -> x*5 + d) 0 . map score) . catMaybes . map closingSequence
