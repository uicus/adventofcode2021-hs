module Day4.Common
     ( read4
     , Board
     , bestResult
     ) where

import System.IO
import Control.Monad.Loops
import Data.List.Split
import Data.List
import qualified Data.Set as Set

data Board = Board { rows :: [Set.Set Int], columns :: [Set.Set Int] }

markNumber :: Int -> Board -> Board
markNumber number board = Board { rows = map (Set.delete number) $ rows board, columns = map (Set.delete number) $ columns board }

victory :: Board -> Bool
victory board = any Set.null (rows board) || any Set.null (columns board)

score :: Int -> Board -> Int
score lastMarked board = lastMarked * (foldl' (+) 0 $ map (Set.foldl' (+) 0) $ rows board)

boardHistory :: [Int] -> Board -> [Board]
boardHistory numbers startBoard = tail result
  where
    result = startBoard : zipWith markNumber numbers result

boardResult :: [Int] -> Board -> (Int, Int, Board)
boardResult numbers = head . dropWhile (not . victory . (\(_, _, trd) -> trd)) . zip3 [0..] numbers . boardHistory numbers

bestResult :: ((Int, Int, Board) -> (Int, Int, Board) -> (Int, Int, Board)) -> [Int] -> [Board] -> Int
bestResult criteria numbers boards = score number board
  where
    (_, number, board) = foldl1' criteria $ map (boardResult numbers) boards

readNumbersToDraw :: Handle -> IO [Int]
readNumbersToDraw = fmap (map (read) . wordsBy (==',')) . hGetLine

readBoard :: Handle -> IO Board
readBoard handle = do
  rows <- untilM (fmap (map (read) . wordsBy (==' ')) $ hGetLine handle) (fmap (=='\n') $ hLookAhead handle)
  hGetLine handle
  return Board { rows = map Set.fromList rows, columns = map Set.fromList . transpose $ rows }

read4 :: Handle -> IO ([Int], [Board])
read4 handle = do
  numbersToDraw <- readNumbersToDraw handle
  hGetLine handle
  boards <- untilM (readBoard handle) (hIsEOF handle)
  return (numbersToDraw, boards)
