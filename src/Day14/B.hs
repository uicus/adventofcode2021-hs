module Day14.B
     ( solve14B
     ) where

import Day14.Common

import qualified Data.Map.Strict as Map

solve14B :: (String, Map.Map (Char, Char) Char) -> Int
solve14B (start, rules) = getDifference $ countLetters $ getLettersCountFromString start 40 rules
