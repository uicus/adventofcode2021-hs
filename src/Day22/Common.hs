module Day22.Common
     ( read22
     , Cube
     , Switch
     , Point(..)
     , inside
     , points
     ) where

import System.IO
import Control.Monad.Loops
import Data.Ord
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

data Point = Point { x :: !Int, y :: !Int, z :: !Int }
type Cube = (Point, Point)
data Switch = On | Off deriving (Eq)
data PositionType = Opening | Closing

inside :: (Point -> Int) -> Int -> Cube -> Bool
inside dimension value (start, end) = value >= dimension start && value < dimension end

goToNextPoint :: (Map.Map Int Switch, (Int, Int)) -> (Switch, PositionType, Int, Int) -> (Map.Map Int Switch, (Int, Int))
goToNextPoint (current, (result, lastPosition)) (switch, positionType, index, position) =
  case positionType of
    Opening -> (Map.insert index switch current, (result + toAdd, position))
    Closing -> (Map.delete index current, (result + toAdd, position))
  where
    toAdd = if not (Map.null current) && snd (Map.findMax current) == On then position - lastPosition else 0

pointsCount :: [(Switch, PositionType, Int, Int)] -> Int
pointsCount [] = 0
pointsCount ((on, Opening, index, firstPosition):xs) = fst $ snd $ foldl' goToNextPoint (Map.insert index on Map.empty, (0, firstPosition)) xs

pointsOnLine :: Int -> [(Switch, Cube)] -> Int
pointsOnLine coordY
  = pointsCount
  . sortBy (comparing $ \(_, _, _, val) -> val)
  . concat
  . zipWith (\index (on, (start, end)) -> [(on, Opening, index, x start), (on, Closing, index, x end)]) [0..]
  . filter (inside y coordY . snd)

pointsOnPlane :: Int -> [(Switch, Cube)] -> Int
pointsOnPlane coordZ cubes
  =
  if null relevantCubes
    then 0
    else
      fst
    $ foldl1' (\(result, lastPosition) (count, position) -> (result + count * (lastPosition - position), position))
    $ reverse
    $ map (\y -> (pointsOnLine y relevantCubes, y))
    $ sort
    $ concat
    $ map (\cube -> [y $ fst $ snd cube, y $ snd $ snd cube]) relevantCubes
  where
    relevantCubes = filter (inside z coordZ . snd) cubes

points :: [(Switch, Cube)] -> Int
points cubes
  = fst
  $ foldl1' (\(result, lastPosition) (count, position) -> (result + count * (lastPosition - position), position))
  $ reverse
  $ map (\z -> (pointsOnPlane z cubes, z))
  $ sort
  $ concat
  $ map (\cube -> [z $ fst $ snd cube, z $ snd $ snd $ cube]) cubes

parseSegment :: String -> (Int, Int)
parseSegment raw = (read rawStart, read rawEnd + 1)
  where
    [rawStart, rawEnd] = splitOn ".." raw

parseCube :: String -> Cube
parseCube raw = (Point { x = startX, y = startY, z = startZ }, Point { x = endX, y = endY, z = endZ })
  where
    [(startX, endX), (startY, endY), (startZ, endZ)] = map (parseSegment . (!!1) . wordsBy (=='=')) $ wordsBy (==',') raw

parseCommand :: String -> (Switch, Cube)
parseCommand raw = (if rawCommand == "on" then On else Off, parseCube rawCube)
  where
    [rawCommand, rawCube] = words raw

read22 :: Handle -> IO [(Switch, Cube)]
read22 handle = untilM (fmap parseCommand $ hGetLine handle) (hIsEOF handle)
