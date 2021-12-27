module Day14.A
     ( solve14A
     ) where

import Day14.Common

import qualified Data.Map.Strict as Map

solve14A :: (String, Map.Map (Char, Char) Char) -> Int
solve14A (start, rules) = getDifference $ countLetters $ getLettersCountFromString start 10 rules
