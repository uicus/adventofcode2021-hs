module Day19.A
     ( solve19A
     ) where

import Day19.Common

import qualified Data.Set as Set

solve19A :: [(Int, [Coords])] -> Int
solve19A = Set.size . Set.fromList . concat . map snd . createMap
