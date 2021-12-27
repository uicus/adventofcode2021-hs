module Day16.B
     ( solve16B
     ) where

import Day16.Common

import Data.List
import Control.Monad.State

idToOperation :: Int -> (Int -> Int -> Int)
idToOperation 0 = (+)
idToOperation 1 = (*)
idToOperation 2 = min
idToOperation 3 = max
idToOperation 5 = (\x y -> fromEnum $ x > y)
idToOperation 6 = (\x y -> fromEnum $ x < y)
idToOperation 7 = (\x y -> fromEnum $ x == y)

value :: Packet -> Int
value packet =
  case content packet of
    Literal x             -> x
    Operator lowerPackets -> foldl1' (idToOperation $ identifier packet) $ map value lowerPackets

solve16B :: String -> Int
solve16B = value . fst . runState parsePacket . flip zip [0..] . toBitString
