module Day19.Common
     ( read19
     , Coords
     , createMap
     ) where

import System.IO
import Control.Monad
import Control.Monad.Loops
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map.Strict as Map

type Coords = (Int, Int, Int)

rotationAroundX :: Coords -> [Coords]
rotationAroundX (x, y, z) = [(x, y, z), (x, z, -y), (x, -y, -z), (x, -z, y)]

rotationAlongX :: Coords -> [Coords]
rotationAlongX (x, y, z) = rotationAroundX (x, y, z) ++ rotationAroundX (-x, y, -z)

possibleOrientations :: Coords -> [Coords]
possibleOrientations (x, y, z) = rotationAlongX (x, y, z) ++ rotationAlongX (y, z, x) ++ rotationAlongX (z, x, y)

move :: Coords -> Coords -> Coords
move (x, y, z) (dx, dy, dz) = (x+dx, y+dy, z+dz)

moveBack :: Coords -> Coords -> Coords
moveBack (x, y, z) (dx, dy, dz) = (x-dx, y-dy, z-dz)

inside :: Int -> Coords -> Coords -> Bool
inside radius (centerX, centerY, centerZ) (beamX, beamY, beamZ) = all (<=radius) [(abs $ centerX - beamX), (abs $ centerY - beamY), (abs $ centerZ - beamZ)]

matches :: Coords -> [Coords] -> Coords -> [Coords] -> Bool
matches center1 beams1 center2 beams2 = length possiblyVisibleBy1 >= 12 && length possiblyVisibleBy2 >= 12 && sort possiblyVisibleBy1 == sort possiblyVisibleBy2
  where
    possiblyVisibleBy1 = filter (inside 1000 center1) beams2
    possiblyVisibleBy2 = filter (inside 1000 center2) beams1

matchConstOrientation :: Coords -> [Coords] -> [Coords] -> Maybe (Coords, [Coords])
matchConstOrientation center1 beams1 beams2
  = find (uncurry $ matches center1 beams1)
  $ map (\center2 -> (center2, map (move center2) beams2))
  $ (moveBack <$> beams1 <*> beams2)

match :: Coords -> [Coords] -> [Coords] -> Maybe (Coords, [Coords])
match center1 beams1 beams2 = msum $ map (matchConstOrientation center1 beams1) $ transpose $ map possibleOrientations beams2

findMatching :: Coords -> [Coords] -> [(Int, [Coords])] -> [(Int, (Coords, [Coords]))]
findMatching previousCenter previousBeams = catMaybes . map (\(index, nextBeams) -> fmap ((,) index) $ match previousCenter previousBeams nextBeams)

moveToFound :: Int -> Coords -> [Coords] -> ([(Coords, [Coords])], [(Int, [Coords])]) -> ([(Coords, [Coords])], [(Int, [Coords])])
moveToFound index nextCenter nextBeams (found, remaining) = ((nextCenter, nextBeams):found, filter ((/=index) . fst) remaining)

takeNextScanner :: Coords -> [Coords] -> ([(Coords, [Coords])], [(Int, [Coords])]) -> ([(Coords, [Coords])], [(Int, [Coords])])
takeNextScanner _ _ (found, []) = (found, [])
takeNextScanner previousCenter previousBeams result@(found, remaining)
  = foldl' (\previousResult (index, (nextCenter, nextBeams)) -> takeNextScanner nextCenter nextBeams $ moveToFound index nextCenter nextBeams previousResult) result
  $ findMatching previousCenter previousBeams remaining

createMap :: [(Int, [Coords])] -> [(Coords, [Coords])]
createMap scanners = fst $ takeNextScanner (0, 0, 0) (snd $ head scanners) ([((0, 0, 0), (snd $ head scanners))], tail scanners)

parseScannerNumber :: String -> Int
parseScannerNumber = read . filter isDigit

parseTriplet :: String -> Coords
parseTriplet raw = (x, y, z)
  where
    [x, y, z] = map read $ wordsBy (==',') raw

readScanner :: Handle -> IO (Int, [Coords])
readScanner handle = (,) <$> (fmap parseScannerNumber $ hGetLine handle) <*> untilM (fmap parseTriplet $ hGetLine handle) (fmap (=='\n') $ hLookAhead handle)

read19 :: Handle -> IO [(Int, [Coords])]
read19 handle = untilM (readScanner handle <* hGetLine handle) (hIsEOF handle)
