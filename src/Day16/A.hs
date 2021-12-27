module Day16.A
     ( solve16A
     ) where

import Day16.Common

import Data.List
import Control.Monad.State

sumOfVersions :: Packet -> Int
sumOfVersions packet = lowerSums `seq` (lowerSums + version packet)
  where
    lowerSums =
      case content packet of
        Literal _             -> 0
        Operator lowerPackets -> foldl' (+) 0 $ map sumOfVersions lowerPackets

solve16A :: String -> Int
solve16A = sumOfVersions . fst . runState parsePacket . flip zip [0..] . toBitString
