module Utils
     ( forceElements
     , digits
     , boolsToInt
     , mapFst
     , neighborsCoordinatesNonDiagonal
     , neighborsCoordinatesWithoutCheck
     , neighborsCoordinates
     , intoMap
     , unfoldWhileIncludingFinalM
     , square
     ) where

import Data.List
import qualified Data.Map.Strict as Map

forceElements :: [a] -> ()
forceElements = foldr seq ()

digits :: String -> [Int]
digits = map (read . pure)

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' (\x b -> x * 2 + fromEnum b) 0

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

neighborsDeltasNonDiagonal :: [(Int, Int)]
neighborsDeltasNonDiagonal = [(y, x) | y <- [-1 .. 1], x <- [-1 .. 1], abs (x + y) == 1]

neighborsCoordinatesNonDiagonal :: Map.Map (Int, Int) a -> (Int, Int) -> [(Int, Int)]
neighborsCoordinatesNonDiagonal grid (y, x) = filter (flip Map.member grid) $ map (\(dy, dx) -> (y+dy, x+dx)) neighborsDeltasNonDiagonal

neighborsDeltas :: [(Int, Int)]
neighborsDeltas = [(y, x) | y <- [-1 .. 1], x <- [-1 .. 1], x /= 0 || y /= 0]

neighborsCoordinatesWithoutCheck :: (Int, Int) -> [(Int, Int)]
neighborsCoordinatesWithoutCheck (y, x) = map (\(dy, dx) -> (y+dy, x+dx)) neighborsDeltas

neighborsCoordinates :: Map.Map (Int, Int) a -> (Int, Int) -> [(Int, Int)]
neighborsCoordinates grid = filter (flip Map.member grid) . neighborsCoordinatesWithoutCheck

intoMap :: [[a]] -> Map.Map (Int, Int) a
intoMap = Map.fromList . concat . map (uncurry $ \indexY -> map (mapFst $ (,) indexY) . zip [0..]) . zip [0..]

unfoldWhileIncludingFinalM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileIncludingFinalM p m = loop id
  where
    loop f = do
      x <- m
      if p x
        then loop (f . (x:))
        else return (f [x])

square :: [a] -> [(a, a)]
square l = (,) <$> l <*> l
