module Day6.Common
     ( read6
     , countFish
     , tick
     , listToFishGenerations
     ) where

import System.IO
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

data FishSet = FishSet { timeToFishCount :: Map.Map Int Int, currentTime :: Int }
data FishGenerations = FishGenerations { mature :: FishSet, kindergarten :: FishSet }

addFish :: Map.Map Int Int -> Int -> Map.Map Int Int
addFish = flip $ flip (Map.insertWith (+)) 1

listToFishSet :: [Int] -> FishSet
listToFishSet startList = FishSet { timeToFishCount = foldl' addFish Map.empty startList, currentTime = 0 }

listToFishGenerations :: [Int] -> FishGenerations
listToFishGenerations startList = FishGenerations { mature = listToFishSet startList, kindergarten = FishSet { timeToFishCount = Map.empty, currentTime = 0 } }

tickMatureTime :: FishSet -> (FishSet, Int)
tickMatureTime fishSet =
  ( fishSet { currentTime = currentTime fishSet + 1 }
  , Map.findWithDefault 0 (currentTime fishSet `mod` 7) (timeToFishCount fishSet)
  )

tickKindergartenTime :: FishSet -> Int -> (FishSet, Int)
tickKindergartenTime fishSet newFish =
  ( fishSet { timeToFishCount = Map.insert (currentTime fishSet `mod` 2) newFish $ timeToFishCount fishSet, currentTime = currentTime fishSet + 1 }
  , Map.findWithDefault 0 (currentTime fishSet `mod` 2) (timeToFishCount fishSet)
  )

insertMatured :: FishSet -> Int -> FishSet
insertMatured fishSet matured = fishSet { timeToFishCount = Map.insertWith (+) ((currentTime fishSet - 1) `mod` 7) matured (timeToFishCount fishSet) }

tick :: FishGenerations -> FishGenerations
tick fishGeneration = FishGenerations { mature = insertMatured newMature matured, kindergarten = newKindergarten }
  where
    (newMature, kids) = tickMatureTime $ mature fishGeneration
    (newKindergarten, matured) = tickKindergartenTime (kindergarten fishGeneration) kids

countFish :: FishGenerations -> Int
countFish fishGeneration = Map.foldl (+) 0 (timeToFishCount $ mature fishGeneration) + Map.foldl (+) 0 (timeToFishCount $ kindergarten fishGeneration)

read6 :: Handle -> IO [Int]
read6 = fmap (map read . wordsBy (==',')) . hGetLine
