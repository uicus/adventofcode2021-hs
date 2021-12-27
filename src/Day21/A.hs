module Day21.A
     ( solve21A
     ) where

import Day21.Common

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad

result ::  (Positions, Results, Bool) -> Maybe Int
result ((_, _), (player1Result, player2Result), _) = if max player1Result player2Result >= 1000 then Just $ min player1Result player2Result else Nothing

game :: [Int] -> (Positions, Results, Bool) -> [(Positions, Results, Bool)]
game dice startPosition = map fst res
  where
    res = zip (startPosition : map (uncurry applyToEntry) res) $ map sum $ chunksOf 3 dice

solve21A :: Positions -> Int
solve21A = fromJust . msum . map (uncurry $ \count -> fmap (*count)) . zip [0, 3..] . map result . game (cycle [1 .. 100]) . start
