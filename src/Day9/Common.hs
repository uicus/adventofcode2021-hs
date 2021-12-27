module Day9.Common
     ( read9
     , neighborsCoordinatesNonDiagonal
     , isLowPoint
     ) where

import Utils

import System.IO
import Control.Monad.Loops
import Data.Maybe
import qualified Data.Map.Strict as Map

cellContent :: Int -> Int -> Map.Map (Int, Int) Int -> Maybe Int
cellContent = curry Map.lookup

neighborsContent :: Map.Map (Int, Int) Int -> (Int, Int) -> [Int]
neighborsContent grid = catMaybes . map (flip Map.lookup grid) . neighborsCoordinatesNonDiagonal grid

isLowPoint :: Map.Map (Int, Int) Int -> (Int, Int) -> Bool
isLowPoint grid coordinates = all ((<) $ fromJust $ Map.lookup coordinates grid) $ neighborsContent grid coordinates

read9 :: Handle -> IO [[Int]]
read9 handle = untilM (fmap digits $ hGetLine handle) (hIsEOF handle)
