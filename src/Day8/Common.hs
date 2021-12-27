module Day8.Common
     ( read8
     ) where

import System.IO
import Data.List.Split
import Control.Monad.Loops

parseLine :: String -> ([String], [String])
parseLine raw = (words left, words right)
  where
    [left, right] = wordsBy (=='|') raw

read8 :: Handle -> IO [([String], [String])]
read8 handle = untilM (fmap parseLine $ hGetLine handle) (hIsEOF handle)
