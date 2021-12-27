module Day13.B
     ( solve13B
     ) where

import Day13.Common

import Data.List
import qualified Data.Set as Set

draw :: [(Int, Int)] -> String
draw points = unlines [[if Set.member (x, y) dots then '#' else ' ' | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    minX = foldl1' min $ map fst points
    maxX = foldl1' max $ map fst points
    minY = foldl1' min $ map snd points
    maxY = foldl1' max $ map snd points
    dots = Set.fromList points

solve13B :: ([(Int, Int)], [Fold]) -> String
solve13B (points, folds) = draw $ foldl' (flip $ \fold -> map (performFold fold)) points folds
