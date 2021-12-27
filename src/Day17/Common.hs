module Day17.Common
     ( read17
     , simulateY
     , possibleYs
     , possibleXs
     , isYAreaReachable
     , isAreaReachable
     ) where

import System.IO
import Data.List.Split
import Data.Maybe
import Data.List

simulateY :: (Int, Int) -> (Int, Int)
simulateY (position, velocity) = (position+velocity, velocity-1)

possibleYs :: (Int, Int) -> [Int]
possibleYs (lowestY, _) = [lowestY .. -lowestY]

simulateX :: (Int, Int) -> (Int, Int)
simulateX (position, velocity) = (position+velocity, max (velocity-1) 0)

possibleXs :: (Int, Int) -> [Int]
possibleXs (_, highestX) = [1 .. highestX]

simulateCoords :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
simulateCoords (x, y) = (simulateX x, simulateY y)

isAreaXReached :: (Int, Int) -> (Int, Int) -> Maybe Bool
isAreaXReached (position, velocity) (lowestX, highestX) =
  if position < lowestX && velocity <= 0
    then Just False
    else
      if position > highestX
        then Just False
        else
          if position >= lowestX && position <= highestX
            then Just True
            else Nothing

isAreaYReached :: (Int, Int) -> (Int, Int) -> Maybe Bool
isAreaYReached (position, _) (lowestY, highestY) =
  if position < lowestY
    then Just False
    else
      if position >= lowestY && position <= highestY
        then Just True
        else Nothing

areaReached :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Maybe Bool
areaReached (coordsX, coordsY) (areaX, areaY) = (&&) <$> isAreaXReached coordsX areaX <*> isAreaYReached coordsY areaY

isYAreaReachable :: Int -> (Int, Int) -> Bool
isYAreaReachable velocityY areaY = fromJust $ head $ dropWhile isNothing $ map (flip isAreaYReached areaY) $ iterate simulateY (0, velocityY)

isAreaReachable :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
isAreaReachable (velocityX, velocityY) area = fromJust $ head $ dropWhile isNothing $ map (flip areaReached area) $ iterate simulateCoords ((0, velocityX), (0, velocityY))

parseEntry :: String -> (Int, Int)
parseEntry raw = (start, end)
  where
    [_, useful] = wordsBy (=='=') raw
    [start, end] = map read $ splitOn ".." useful

parseArea :: String -> ((Int, Int), (Int, Int))
parseArea raw = (parseEntry entryX, parseEntry entryY)
  where
    [_, useful] = wordsBy (==':') raw
    [entryX, entryY] = wordsBy (==',') useful

read17 :: Handle -> IO ((Int, Int), (Int, Int))
read17 = fmap parseArea . hGetLine
