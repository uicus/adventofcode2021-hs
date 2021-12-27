module Day20.Common
     ( read20
     , step
     ) where

import Utils

import System.IO
import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

coordsToLookup :: (Int, Int) -> [(Int, Int)]
coordsToLookup coords = sort $ coords:neighborsCoordinatesWithoutCheck coords

resultBit :: Map.Map (Int, Int) Bool -> Map.Map Int Bool -> Bool -> (Int, Int) -> Bool
resultBit image rules infiniteBit
  = fromJust
  . flip Map.lookup rules
  . boolsToInt
  . catMaybes
  . map (\coords -> Map.lookup coords image `mplus` Just infiniteBit)
  . coordsToLookup

interestingBits :: Map.Map (Int, Int) Bool -> [(Int, Int)]
interestingBits = Set.toList . Set.fromList . concat . map (coordsToLookup . fst) . Map.toList

flipInfiniteBit :: Map.Map Int Bool -> Bool -> Bool
flipInfiniteBit rules = fromJust . flip Map.lookup rules . boolsToInt . replicate 9

step :: Map.Map Int Bool -> (Map.Map (Int, Int) Bool, Bool) -> (Map.Map (Int, Int) Bool, Bool)
step rules (image, infiniteBit) =
  ( Map.fromList $ map (\coords -> (coords, resultBit image rules infiniteBit coords)) $ interestingBits image
  , flipInfiniteBit rules infiniteBit
  )

charToBool :: Char -> Bool
charToBool '#' = True
charToBool '.' = False

readAlgorithm :: Handle -> IO (Map.Map Int Bool)
readAlgorithm = fmap (Map.fromList . zip [0..] . map charToBool) . hGetLine

readImage :: Handle -> IO (Map.Map (Int, Int) Bool)
readImage handle = fmap intoMap $ untilM (fmap (map charToBool) $ hGetLine handle) (hIsEOF handle)

read20 :: Handle -> IO (Map.Map Int Bool, Map.Map (Int, Int) Bool)
read20 handle = (,) <$> readAlgorithm handle <* hGetLine handle <*> readImage handle
