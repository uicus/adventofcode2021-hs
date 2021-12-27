module Day18.Common
     ( read18
     , Number(..)
     , add
     , magnitude
     ) where

import Utils

import System.IO
import Control.Monad.Loops
import Control.Monad.State
import Data.Char
import Data.Maybe

data Number
  = Regular !Int
  | Pair Number Number

modifyRightmost :: (Int -> Int) -> Number -> Number
modifyRightmost f (Regular x) = Regular $ f x
modifyRightmost f (Pair left right) = Pair left $ modifyRightmost f right

modifyLeftmost :: (Int -> Int) -> Number -> Number
modifyLeftmost f (Regular x) = Regular $ f x
modifyLeftmost f (Pair left right) = Pair (modifyLeftmost f left) right

addAtRight :: Maybe Int -> Number -> Number
addAtRight Nothing = id
addAtRight (Just x) = modifyLeftmost (+x)

addAtLeft :: Maybe Int -> Number -> Number
addAtLeft Nothing = id
addAtLeft (Just x) = modifyRightmost (+x)

explodeAtLvl :: Int -> Number -> Maybe (Number, (Maybe Int, Maybe Int))
explodeAtLvl _ (Regular _) = Nothing
explodeAtLvl lvl (Pair (Regular left) (Regular right)) =
  if lvl >= 4
    then Just (Regular 0, (Just left, Just right))
    else Nothing
explodeAtLvl lvl (Pair left right) = fmap handleLeft (explodeAtLvl (lvl+1) left) `mplus` fmap handleRight (explodeAtLvl (lvl+1) right)
  where
    handleLeft (newLeft, (toAddAtLeft, toAddAtRight)) = (Pair newLeft $ addAtRight toAddAtRight right, (toAddAtLeft, Nothing))
    handleRight (newRight, (toAddAtLeft, toAddAtRight)) = (Pair (addAtLeft toAddAtLeft left) newRight, (Nothing, toAddAtRight))

explode :: Number -> Maybe Number
explode = fmap fst . explodeAtLvl 0

split :: Number -> Maybe Number
split (Regular x) = if x >= 10 then Just $ Pair (Regular $ x `div` 2) (Regular $ (x+1) `div` 2) else Nothing
split (Pair left right) = fmap (flip Pair right) (split left) `mplus` fmap (Pair left) (split right)

reduceOnce :: Number -> Maybe Number
reduceOnce n = explode n `mplus` split n

reduce :: Number -> Number
reduce n = fromJust $ last $ takeWhile isJust steps
  where
    steps = Just n : map (>>= reduceOnce) steps

add :: Number -> Number -> Number
add n1 n2 = reduce $ Pair n1 n2

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair left right) = 3*magnitude left + 2*magnitude right

type ParserState = String

parseRegular :: State ParserState Number
parseRegular = state $ mapFst (Regular . read) . span isDigit

discardCharacter :: State ParserState ()
discardCharacter = state $ (,) () . tail

parsePair :: State ParserState Number
parsePair = Pair <$ discardCharacter <*> parseNumber <* discardCharacter <*> parseNumber <* discardCharacter

isInteger :: State ParserState Bool
isInteger = fmap (isDigit . head) get

parseNumber :: State ParserState Number
parseNumber = do
  isInt <- isInteger
  if isInt
    then parseRegular
    else parsePair

read18 :: Handle -> IO [Number]
read18 handle = untilM (fmap (fst . runState parseNumber) $ hGetLine handle) (hIsEOF handle)
