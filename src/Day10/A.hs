module Day10.A
     ( solve10A
     ) where

import Day10.Common

import Data.List
import Data.Maybe
import Data.Either.Combinators

illegalCharacter :: String -> Maybe Char
illegalCharacter = leftToMaybe . parse []

score :: Char -> Int
score ')' = 3
score '}' = 1197
score ']' = 57
score '>' = 25137

solve10A :: [String] -> Int
solve10A = foldl' (+) 0 . map score . catMaybes . map illegalCharacter
