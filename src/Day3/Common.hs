module Day3.Common
     ( read3
     , Result(..)
     , applyBit
     , chooseLargest
     , chooseLowest
     ) where

import System.IO
import Control.Monad.Loops
import Data.List

data Result = Result { falses :: !Int, trues :: !Int }

applyBit :: Result -> Bool -> Result
applyBit res True = res { trues = trues res + 1 }
applyBit res False = res { falses = falses res + 1 }

chooseLargest :: Result -> Bool
chooseLargest res = falses res <= trues res

chooseLowest :: Result -> Bool
chooseLowest res = falses res > trues res

charToBool :: Char -> Bool
charToBool '0' = False
charToBool '1' = True
charToBool _ = undefined

read3 :: Handle -> IO [[Bool]]
read3 handle = untilM (fmap (map charToBool) $ hGetLine handle) (hIsEOF handle)
