module Day21.Common
     ( read21
     , Positions
     , Results
     , start
     , applyToEntry
     ) where

import System.IO

type Positions = (Int, Int)
type Results = (Int, Int)

start :: Positions -> (Positions, Results, Bool)
start positions = (positions, (0, 0), True)

move :: Int -> Int -> Int
move start change = ((start+change-1) `mod` 10)+1

applyToEntry :: (Positions, Results, Bool) -> Int -> (Positions, Results, Bool)
applyToEntry ((position1, position2), (result1, result2), player1Turn) change =
  if player1Turn
    then let newPosition = move position1 change in ((newPosition, position2), (result1+newPosition, result2), not player1Turn)
    else let newPosition = move position2 change in ((position1, newPosition), (result1, result2+newPosition), not player1Turn)

readPosition :: Handle -> IO Int
readPosition = fmap (read . last . words) . hGetLine

read21 :: Handle -> IO Positions
read21 handle = (,) <$> readPosition handle <*> readPosition handle
