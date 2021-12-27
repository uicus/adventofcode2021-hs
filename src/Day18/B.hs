module Day18.B
     ( solve18B
     ) where

import Day18.Common
import Utils

import Data.List

solve18B :: [Number] -> Int
solve18B = foldl' max 0 . map (magnitude . uncurry add) . square
