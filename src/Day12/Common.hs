module Day12.Common
     ( read12
     ) where

import System.IO
import Control.Monad.Loops
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

readPair :: String -> (String, String)
readPair raw = (a, b)
  where
    [a, b] = wordsBy (=='-') raw

getId :: Map.Map String Int -> String -> (Map.Map String Int, Int)
getId names name =
  case alreadyAssigned of
    Just x  -> (names, x)
    Nothing -> (Map.insert name (Map.size names) names, Map.size names)
  where
    alreadyAssigned = Map.lookup name names

addEdge :: (Map.Map String Int, Map.Map Int [Int]) -> (String, String) -> (Map.Map String Int, Map.Map Int [Int])
addEdge (names, graph) (a, b) = (namesWithAB, graphWithAB)
  where
    (namesWithA, idA) = getId names a
    (namesWithAB, idB) = getId namesWithA b
    graphWithA = Map.insertWith (++) idA [idB] graph
    graphWithAB = Map.insertWith (++) idB [idA] graphWithA

isWholeUpper :: String -> Bool
isWholeUpper = all isUpper

buildGraph :: (Map.Map String Int, Map.Map Int [Int]) -> (Map.Map Int [Int], Set.Set Int, Int, Int)
buildGraph (names, graph) =
  ( graph
  , Set.fromList $ Map.elems $ Map.filterWithKey (const . isWholeUpper) names
  , fromJust $ Map.lookup "start" names
  , fromJust $ Map.lookup "end" names
  )

edgeListToGraph :: [(String, String)] -> (Map.Map Int [Int], Set.Set Int, Int, Int)
edgeListToGraph = buildGraph . foldl' addEdge (Map.empty, Map.empty)

read12 :: Handle -> IO (Map.Map Int [Int], Set.Set Int, Int, Int)
read12 handle = fmap edgeListToGraph $ untilM (fmap readPair $ hGetLine handle) (hIsEOF handle)
