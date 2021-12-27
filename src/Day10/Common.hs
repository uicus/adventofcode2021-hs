module Day10.Common
     ( read10
     , parse
     , correspondingBracket
     ) where

import System.IO
import Control.Monad.Loops

opening :: Char -> Bool
opening = flip elem ['(', '{', '[', '<']

correspondingBracket :: Char -> Char
correspondingBracket '(' = ')'
correspondingBracket '{' = '}'
correspondingBracket '[' = ']'
correspondingBracket '<' = '>'

matches :: [Char] -> Char -> Bool
matches [] _ = False
matches (x:xs) next = correspondingBracket x == next

parse :: [Char] -> String -> Either Char [Char]
parse stack [] = Right stack
parse stack (x:xs) =
  if opening x
    then parse (x:stack) xs
    else
      if matches stack x
        then parse (tail stack) xs
        else Left x

read10 :: Handle -> IO [String]
read10 handle = untilM (hGetLine handle) (hIsEOF handle)
