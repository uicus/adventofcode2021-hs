module Day11.Common
     ( read11
     , collectResult
     ) where

import Utils

import System.IO
import Control.Monad.Loops
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

incrementAll :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
incrementAll = Map.map (+1)

incrementChosen :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Map.Map (Int, Int) Int
incrementChosen grid = foldl' (flip $ Map.adjust (+1)) grid . concat . map (neighborsCoordinates grid)

flashingSubstep :: Set.Set (Int, Int) -> Map.Map (Int, Int) Int -> [(Int, Int)]
flashingSubstep alreadyFlashed = map fst . Map.toList . Map.filterWithKey (\coords energy -> Set.notMember coords alreadyFlashed && energy > 9)

resetFlashed :: Map.Map (Int, Int) Int -> Set.Set (Int, Int) -> Map.Map (Int, Int) Int
resetFlashed grid = foldl' (flip $ Map.adjust (const 0)) grid . Set.toList

substeps :: Set.Set (Int, Int) -> Map.Map (Int, Int) Int -> (Int, Map.Map (Int, Int) Int)
substeps alreadyFlashed grid =
  if null nextToFlash
    then (Set.size alreadyFlashed, resetFlashed grid alreadyFlashed)
    else substeps (Set.union alreadyFlashed $ Set.fromList nextToFlash) (incrementChosen grid nextToFlash)
  where
    nextToFlash = flashingSubstep alreadyFlashed grid

step :: Map.Map (Int, Int) Int -> (Int, Map.Map (Int, Int) Int)
step = substeps Set.empty . incrementAll

collectResult :: Map.Map (Int, Int) Int -> [Int]
collectResult grid = map fst $ tail results
  where
    results = (0, grid) : map (step . snd) results

read11 :: Handle -> IO [[Int]]
read11 handle = untilM (fmap digits $ hGetLine handle) (hIsEOF handle)
