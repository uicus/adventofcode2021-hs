module Day25.Common
     ( read25
     , Tile(..)
     ) where

import System.IO
import Control.Monad.Loops

data Tile = Empty | South | East

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile 'v' = South
parseTile '>' = East

read25 :: Handle -> IO [[Tile]]
read25 handle = untilM (fmap (map parseTile) $ hGetLine handle) (hIsEOF handle)
