module Day14.Common
     ( read14
     , getDifference
     , countLetters
     , getLettersCountFromString
     ) where

import System.IO
import Control.Monad.Loops
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map

type LettersCount = Map.Map Char Int

addLetter :: Char -> LettersCount -> LettersCount
addLetter = flip (Map.insertWith (+)) 1

joinLetters :: LettersCount -> LettersCount -> LettersCount
joinLetters = Map.unionWith (+)

countLetters :: LettersCount -> [(Int, Char)]
countLetters = reverse . sort . map swap . Map.toList

getDifference :: [(Int, Char)] -> Int
getDifference resultList = (fst $ head resultList) - (fst $ last resultList)

gatherAllLetters :: Map.Map (Char, Char) Char -> [Char]
gatherAllLetters = nub . concat . map (\((a, b), c) -> [a, b, c]) . Map.toList

getLettersCountFromString :: String -> Int -> Map.Map (Char, Char) Char -> LettersCount
getLettersCountFromString start depth rules = foldl' (flip $ joinLetters . getResult) topLevelLetters $ flip zip (repeat depth) $ zip start (tail start)
  where
    topLevelLetters = foldl' (flip addLetter) Map.empty start
    calculateResult (charPair@(left, right), depth) =
      if depth == 0
        then Map.empty
        else addLetter middle $ joinLetters (getResult ((left, middle), (depth-1))) (getResult ((middle, right), (depth-1)))
      where
        middle = fromJust $ Map.lookup charPair rules
    getResult = fromJust . flip Map.lookup entries
      where
        allLetters = gatherAllLetters rules
        keys = [((a, b), c) | a <- allLetters, b <- allLetters, c <- [0 .. depth]]
        entries = Map.fromList $ [(key, calculateResult key) | key <- keys]

readStart :: Handle -> IO String
readStart = hGetLine

readPair :: String -> ((Char, Char), Char)
readPair raw = ((char1, char2), char3)
  where
    [[char1, char2], [char3]] = splitOn " -> " raw

readRules :: Handle -> IO (Map.Map (Char, Char) Char)
readRules handle = fmap Map.fromList $ hGetLine handle >> untilM (fmap readPair $ hGetLine handle) (hIsEOF handle)

read14 :: Handle -> IO (String, Map.Map (Char, Char) Char)
read14 handle = (,) <$> readStart handle <*> readRules handle
