module Day13.A
     ( solve13A
     ) where

import Day13.Common

import Data.List

solve13A :: ([(Int, Int)], [Fold]) -> Int
solve13A (points, fold:_) = length $ nub $ map (performFold fold) points
