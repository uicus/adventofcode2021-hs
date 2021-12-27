module Day13.Common
     ( read13
     , Fold
     , performFold
     ) where

import System.IO
import Control.Monad.Loops
import Data.List.Split

data Fold
  = Vertical Int
  | Horizontal Int

readPair :: String -> (Int, Int)
readPair raw = (read a, read b)
  where
    [a, b] = wordsBy (==',') raw

performFold :: Fold -> (Int, Int) -> (Int, Int)
performFold (Vertical xFold) (x, y) = (if x < xFold then x else (2*xFold-x), y)
performFold (Horizontal yFold) (x, y) = (x, if y < yFold then y else (2*yFold-y))

readPoints :: Handle -> IO [(Int, Int)]
readPoints handle = untilM (fmap readPair $ hGetLine handle) (fmap (=='\n') $ hLookAhead handle)

readFold :: String -> Fold
readFold raw =
  if dir == "x"
    then Vertical $ read value
    else Horizontal $ read value
  where
    [dir, value] = wordsBy (=='=') $ last $ words raw

readFolds :: Handle -> IO [Fold]
readFolds handle = hGetLine handle >> untilM (fmap readFold $ hGetLine handle) (hIsEOF handle)

read13 :: Handle -> IO ([(Int, Int)], [Fold])
read13 handle = (,) <$> readPoints handle <*> readFolds handle
