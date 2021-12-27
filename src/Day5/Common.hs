module Day5.Common
     ( read5
     , Line(..)
     , vertical
     , horizontal
     , countIntersections
     ) where

import System.IO
import Control.Monad.Loops
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

data Point = Point { x :: !Int, y :: !Int } deriving ( Ord, Eq )
data Line = Line { start :: !Point, end :: !Point }

vertical :: Point -> Point -> Bool
vertical point1 point2 = x point1 == x point2

horizontal :: Point -> Point -> Bool
horizontal point1 point2 = y point1 == y point2

rangeDescending :: Int -> Int -> [Int]
rangeDescending x y =
  if x >= y
    then x:rangeDescending (x-1) y
    else []

allNumbersBetween :: Int -> Int -> [Int]
allNumbersBetween n1 n2 =
  if n1 > n2
    then rangeDescending n1 n2
    else
      if n2 > n1
        then [n1 .. n2]
        else repeat n1

allPoints :: Line -> [Point]
allPoints line = zipWith Point (allNumbersBetween (x $ start line) (x $ end line)) (allNumbersBetween (y $ start line) (y $ end line))

addPoint :: Map.Map Point Int -> Point -> Map.Map Point Int
addPoint = flip $ flip (Map.insertWith (+)) 1

largerThanOneCount :: Map.Map Point Int -> Int
largerThanOneCount = Map.foldl' (\count next -> if next > 1 then count + 1 else count) 0

countIntersections :: [Line] -> Int
countIntersections = largerThanOneCount . foldl' addPoint Map.empty . concat . map allPoints

parsePoint :: String -> Point
parsePoint raw = Point { x = read rawX, y = read rawY }
  where
    [rawX, rawY] = wordsBy (==',') raw

parseLine :: String -> Line
parseLine raw = Line { start = parsePoint rawStart, end = parsePoint rawEnd }
  where
    [rawStart, rawEnd] = splitOn " -> " raw

read5 :: Handle -> IO [Line]
read5 handle = untilM (fmap parseLine $ hGetLine handle) (hIsEOF handle)
