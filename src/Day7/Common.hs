module Day7.Common
     ( read7
     ) where

import System.IO
import Data.List.Split

read7 :: Handle -> IO [Int]
read7 = fmap (map read . wordsBy (==',')) . hGetLine
