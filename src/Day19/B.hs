module Day19.B
     ( solve19B
     ) where

import Day19.Common
import Utils

import Data.List

manhattan :: Coords -> Coords -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = (abs $ x1 - x2) + (abs $ y1 - y2) + (abs $ z1 - z2)

solve19B :: [(Int, [Coords])] -> Int
solve19B = foldl1' max . map (uncurry manhattan) . square . map fst . createMap
