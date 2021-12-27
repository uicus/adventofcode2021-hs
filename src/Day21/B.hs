module Day21.B
     ( solve21B
     ) where

import Day21.Common
import Utils

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

count :: [Int] -> [(Int, Int)] -> [(Int, Int)]
count [] = id
count (x:xs) = count xs . map (\(v, c) -> if v == x then (v, c+1) else (v, c))

possibleMoves :: [(Int, Int)]
possibleMoves = flip count (zip [3..9] $ repeat 0) $ (+) <$> ((+) <$> [1, 2, 3] <*> [1, 2, 3]) <*> [1, 2, 3]

trunkate :: (Positions, Results, Bool) -> (Positions, Results, Bool)
trunkate (positions, (result1, result2), player1Turn) = (positions, (min result1 21, min result2 21), player1Turn)

possibleEntries :: (Positions, Results, Bool) -> [((Positions, Results, Bool), Int)]
possibleEntries entry = map (mapFst $ trunkate . applyToEntry entry) possibleMoves

singleResult :: (Positions, Results, Bool) -> Maybe (Int, Int)
singleResult (_, (result1, result2), _) = player1Wins `mplus` player2Wins
  where
    player1Wins = if result1 >= 21 then Just (1, 0) else Nothing
    player2Wins = if result2 >= 21 then Just (0, 1) else Nothing

addResults :: (Int, Int) -> (Int, Int) -> (Int, Int)
addResults (x1, y1) (x2, y2) = (x1+x2, y1+y2)

multiplyResults :: Int -> (Int, Int) -> (Int, Int)
multiplyResults times (x, y) = (x*times, y*times)

calculateResult :: (Positions, Results, Bool) -> (Int, Int)
calculateResult entry =
  case singleResult entry of
    Just x  -> x
    Nothing -> foldl' (flip $ \(entry, count) -> addResults $ multiplyResults count $ getResult entry) (0, 0) $ possibleEntries entry

getResult :: (Positions, Results, Bool) -> (Int, Int)
getResult = fromJust . flip Map.lookup entries
  where
    keys = [((position1, position2), (result1, result2), player1Turn) | position1 <- [1 .. 10]
                                                                      , position2 <- [1 .. 10]
                                                                      , result1 <- [0 .. 21]
                                                                      , result2 <- [0 .. 21]
                                                                      , player1Turn <- [True, False]]
    entries = Map.fromList $ [(key, calculateResult key) | key <- keys]

solve21B :: Positions -> Int
solve21B = uncurry max . getResult . start
