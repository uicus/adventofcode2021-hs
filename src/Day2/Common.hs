module Day2.Common
     ( read2
     , Command(..)
     ) where

import System.IO
import Control.Monad.Loops

data Command
  = Forward Int
  | Down Int
  | Up Int

stringsToCommand :: String -> String -> Command
stringsToCommand commandName commandArgument =
  case commandName of
    "forward" -> Forward $ read commandArgument
    "up"      -> Up $ read commandArgument
    "down"    -> Down $ read commandArgument

parseCommand :: String -> Command
parseCommand raw =
  let [x, y] = words raw
  in stringsToCommand x y

read2 :: Handle -> IO [Command]
read2 handle = untilM (fmap parseCommand $ hGetLine handle) (hIsEOF handle)
