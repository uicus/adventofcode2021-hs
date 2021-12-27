module Day1.Common
     ( read1
     ) where

import System.IO
import Control.Monad.Loops

read1 :: Handle -> IO [Int]
read1 handle = untilM (fmap read $ hGetLine handle) (hIsEOF handle)
