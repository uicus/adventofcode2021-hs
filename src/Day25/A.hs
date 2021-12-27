module Day25.A
     ( solve25A
     ) where

import Day25.Common

import Data.List
import qualified Data.Set as Set

type Occupied = Set.Set (Int, Int)

addTileToSets :: (Occupied, Occupied) -> (Int, Int) -> Tile -> (Occupied, Occupied)
addTileToSets sets _ Empty = sets
addTileToSets (south, east) coords South = (Set.insert coords south, east)
addTileToSets (south, east) coords East = (south, Set.insert coords east)

intoSets :: [[Tile]] -> (Occupied, Occupied)
intoSets grid
  = foldl' (\sets (row, indexY) -> foldl' (\sets (tile, indexX) -> addTileToSets sets (indexY, indexX) tile) sets $ zip row [0..]) (Set.empty, Set.empty)
  $ zip grid [0..]

intoSize :: [[Tile]] -> (Int, Int)
intoSize grid = (length grid, length $ head grid)

move :: (Int, Int) -> (Occupied, Occupied) -> (Int, Int) -> (Int, Int) -> (Int, Int)
move (dy, dx) (south, east) (sizeY, sizeX) coords@(y, x) =
  if Set.notMember newCoords south && Set.notMember newCoords east
    then newCoords
    else coords
  where
    newCoords = ((y+dy) `mod` sizeY, (x+dx) `mod` sizeX)

moveEast :: (Occupied, Occupied) -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveEast = move (0, 1)

moveSouth :: (Occupied, Occupied) -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveSouth = move (1, 0)

moveAllEast :: (Int, Int) -> (Occupied, Occupied) -> (Occupied, Occupied)
moveAllEast size oldMap@(south, east) = (south, Set.map (moveEast oldMap size) east)

moveAllSouth :: (Int, Int) -> (Occupied, Occupied) -> (Occupied, Occupied)
moveAllSouth size oldMap@(south, east) = (Set.map (moveSouth oldMap size) south, east)

step :: (Int, Int) -> (Occupied, Occupied) -> (Occupied, Occupied)
step size = moveAllSouth size . moveAllEast size

end :: (Occupied, Occupied) -> (Occupied, Occupied) -> Bool
end (south1, east1) (south2, east2) = south1 == south2 && east1 == east2

solve25A :: [[Tile]] -> Int
solve25A grid = (\(_, _, index) -> index) $ head $ dropWhile (\(sets, setsPrevious, _) -> not $ end sets setsPrevious) $ zip3 steps (tail steps) [1..]
  where
    size = intoSize grid
    sets = intoSets grid
    steps = iterate (step size) sets
