module Day23.Common
     ( read23
     , Amphipod(..)
     , Sideroom
     , start
     , getMinResult
     ) where

import Utils

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

data Amphipod = A | B | C | D deriving(Eq, Ord, Show)
type Sideroom = [Amphipod]

correctAssignment :: [Amphipod]
correctAssignment = [A, B, C, D]

data BurrowState
  = BurrowState { siderooms :: [[Amphipod]], leftDeadEnd :: [Maybe Amphipod], rightDeadEnd :: [Maybe Amphipod], corridor :: [Maybe Amphipod] }
    deriving(Eq, Ord, Show)

amphipodCost :: Amphipod -> Int
amphipodCost A = 1
amphipodCost B = 10
amphipodCost C = 100
amphipodCost D = 1000

------------------------------------------------------------------

freeSpaceToCorridor :: Int -> Int -> [Maybe Amphipod] -> Bool
freeSpaceToCorridor roomNumber corridorNumber =
  all isNothing . if roomNumber <= corridorNumber
    then take (corridorNumber - roomNumber + 1) . drop roomNumber
    else take (roomNumber - corridorNumber) . drop corridorNumber

freeSpaceToLeftDeadEnd :: Int -> [Maybe Amphipod] -> Bool
freeSpaceToLeftDeadEnd = flip freeSpaceToCorridor 0

freeSpaceToRightDeadEnd :: Int -> [Maybe Amphipod] -> Bool
freeSpaceToRightDeadEnd = flip freeSpaceToCorridor 3

freeSpaceInDeadEnd :: Int -> [Maybe Amphipod] -> Bool
freeSpaceInDeadEnd deadEndNumber = all isNothing . take (deadEndNumber+1)

moveToCorridorCost :: Int -> Int -> Int
moveToCorridorCost roomNumber corridorNumber =
  if roomNumber <= corridorNumber
    then 2*(corridorNumber - roomNumber) + 1
    else 2*(roomNumber - corridorNumber) - 1

------------------------------------------------------------------

moveToCorridor :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveToCorridor roomNumber amphipod burrowState = do
  corridorNumber <- [0 .. length (corridor burrowState) - 1]
  guard $ freeSpaceToCorridor roomNumber corridorNumber $ corridor burrowState
  let (prefix, _:suffix) = splitAt corridorNumber $ corridor burrowState
  return (moveToCorridorCost roomNumber corridorNumber*amphipodCost amphipod, burrowState { corridor = prefix ++ (Just amphipod:suffix) })

moveToLeftDeadEnd :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveToLeftDeadEnd roomNumber amphipod burrowState = do
  deadEndNumber <- [0 .. length (leftDeadEnd burrowState) - 1]
  guard $ freeSpaceInDeadEnd deadEndNumber $ leftDeadEnd burrowState
  guard $ freeSpaceToLeftDeadEnd roomNumber $ corridor burrowState
  let (prefix, _:suffix) = splitAt deadEndNumber $ leftDeadEnd burrowState
  return ((2*roomNumber + 1 + deadEndNumber)*amphipodCost amphipod, burrowState { leftDeadEnd = prefix ++ (Just amphipod:suffix) })

moveToRightDeadEnd :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveToRightDeadEnd roomNumber amphipod burrowState = do
  deadEndNumber <- [0 .. length (rightDeadEnd burrowState) - 1]
  guard $ freeSpaceInDeadEnd deadEndNumber $ rightDeadEnd burrowState
  guard $ freeSpaceToRightDeadEnd roomNumber $ corridor burrowState
  let (prefix, _:suffix) = splitAt deadEndNumber $ rightDeadEnd burrowState
  return ((2 * (3-roomNumber) + 1 + deadEndNumber)*amphipodCost amphipod, burrowState { rightDeadEnd = prefix ++ (Just amphipod:suffix) })

movableFromSiderooms :: Int -> BurrowState -> [(Int, Int, Amphipod, BurrowState)]
movableFromSiderooms desiredLength burrowState = do
  (shouldBe, roomNumber) <- zip correctAssignment [0..]
  let (prefix, room:suffix) = splitAt roomNumber $ siderooms burrowState
  guard $ any (/=shouldBe) room
  let amphipod = head room
  return (roomNumber, (desiredLength - length room + 1)*amphipodCost amphipod, amphipod, burrowState { siderooms = prefix ++ (tail room:suffix) })

moveFromSideroom :: Int -> BurrowState -> [(Int, BurrowState)]
moveFromSideroom desiredLength burrowState = do
  (roomNumber, cost, amphipod, nextState) <- movableFromSiderooms desiredLength burrowState
  map (mapFst (+cost)) $ moveToCorridor roomNumber amphipod nextState <|> moveToLeftDeadEnd roomNumber amphipod nextState <|> moveToRightDeadEnd roomNumber amphipod nextState

------------------------------------------------------------------

moveFromCorridor :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveFromCorridor roomNumber amphipod burrowState = do
  corridorNumber <- [0 .. length (corridor burrowState) - 1]
  let (prefix, content:suffix) = splitAt corridorNumber $ corridor burrowState
  guard $ content == Just amphipod
  let newState = burrowState { corridor = prefix ++ (Nothing:suffix) }
  guard $ freeSpaceToCorridor roomNumber corridorNumber $ corridor newState
  return (moveToCorridorCost roomNumber corridorNumber*amphipodCost amphipod, newState)

moveFromLeftDeadEnd :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveFromLeftDeadEnd roomNumber amphipod burrowState = do
  deadEndNumber <- [0 .. length (leftDeadEnd burrowState) - 1]
  let (prefix, content:suffix) = splitAt deadEndNumber $ leftDeadEnd burrowState
  guard $ content == Just amphipod
  let newState = burrowState { leftDeadEnd = prefix ++ (Nothing:suffix) }
  guard $ freeSpaceInDeadEnd deadEndNumber $ leftDeadEnd newState
  guard $ freeSpaceToLeftDeadEnd roomNumber $ corridor newState
  return ((2*roomNumber + 1 + deadEndNumber)*amphipodCost amphipod, newState)

moveFromRightDeadEnd :: Int -> Amphipod -> BurrowState -> [(Int, BurrowState)]
moveFromRightDeadEnd roomNumber amphipod burrowState = do
  deadEndNumber <- [0 .. length (rightDeadEnd burrowState) - 1]
  let (prefix, content:suffix) = splitAt deadEndNumber $ rightDeadEnd burrowState
  guard $ content == Just amphipod
  let newState = burrowState { rightDeadEnd = prefix ++ (Nothing:suffix) }
  guard $ freeSpaceInDeadEnd deadEndNumber $ rightDeadEnd newState
  guard $ freeSpaceToRightDeadEnd roomNumber $ corridor newState
  return ((2*(3 - roomNumber) + 1 + deadEndNumber)*amphipodCost amphipod, newState)

movableToSiderooms :: Int -> BurrowState -> [(Int, Int, Amphipod, BurrowState)]
movableToSiderooms desiredLength burrowState = do
  (shouldBe, roomNumber) <- zip correctAssignment [0..]
  let (prefix, room:suffix) = splitAt roomNumber $ siderooms burrowState
  guard $ all (==shouldBe) room
  return (roomNumber, (desiredLength - length room)*amphipodCost shouldBe, shouldBe, burrowState { siderooms = prefix ++ ((shouldBe:room):suffix) })

moveToSideroom :: Int -> BurrowState -> [(Int, BurrowState)]
moveToSideroom desiredLength burrowState = do
  (roomNumber, cost, amphipod, nextState) <- movableToSiderooms desiredLength burrowState
  map (mapFst (+cost)) $ moveFromCorridor roomNumber amphipod nextState <|> moveFromLeftDeadEnd roomNumber amphipod nextState <|> moveFromRightDeadEnd roomNumber amphipod nextState

------------------------------------------------------------------

start :: [Sideroom] -> BurrowState
start siderooms = BurrowState { siderooms = siderooms, leftDeadEnd = replicate 2 Nothing, rightDeadEnd = replicate 2 Nothing, corridor = replicate 3 Nothing }

end :: Int -> BurrowState -> Bool
end desiredLength = all id . zipWith (\shouldBe -> (==replicate desiredLength shouldBe)) correctAssignment . siderooms

step :: Int -> BurrowState -> [(Int, BurrowState)]
step desiredLength s = moveFromSideroom desiredLength s <|> moveToSideroom desiredLength s

checkReachable :: Int -> Set.Set BurrowState -> BurrowState -> Set.Set BurrowState
checkReachable desiredLength visited state =
  if Set.member state visited
    then visited
    else foldl' (checkReachable desiredLength) (Set.insert state visited) $ map snd $ step desiredLength state

getMinResult :: BurrowState -> Int
getMinResult state = getResult state
  where
    desiredLength = length $ head $ siderooms state
    keys = Set.toList $ checkReachable desiredLength Set.empty state
    calculateResult state =
      if end desiredLength state
        then 0
        else foldl' min 1000000 $ map (\(stepCost, nextState) -> stepCost + getResult nextState) $ step desiredLength state
    getResult = fromJust . flip Map.lookup entries
      where
        entries = Map.fromList $ map (\k -> (k, calculateResult k)) keys

parseAmphipod :: Char -> Amphipod
parseAmphipod 'A' = A
parseAmphipod 'B' = B
parseAmphipod 'C' = C
parseAmphipod 'D' = D

readLineWithAmphipods :: Handle -> IO [Amphipod]
readLineWithAmphipods = fmap (map parseAmphipod . filter isLetter). hGetLine

read23 :: Handle -> IO [Sideroom]
read23 handle = hGetLine handle >> hGetLine handle >> (fmap transpose $ replicateM 2 $ readLineWithAmphipods handle)
