module Day18.A
     ( solve18A
     ) where

import Day18.Common

import Data.List

solve18A :: [Number] -> Int
solve18A = magnitude . foldl1' add
