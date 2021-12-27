module Day16.Common
     ( read16
     , parsePacket
     , toBitString
     , Packet(..)
     , Content(..)
     ) where

import Utils

import System.IO
import Control.Monad.State
import Control.Applicative
import Control.Monad.Loops
import Data.List

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

charToBool :: Char -> Bool
charToBool '1' = True
charToBool '0' = False

toBitString :: String -> [Bool]
toBitString = map charToBool . concat . map hexToBin

data Content
  = Literal !Int
  | Operator [Packet]

data Packet = Packet { version :: !Int, identifier :: !Int, content :: Content }

type ParserState = [(Bool, Int)]

parseBool :: State ParserState Bool
parseBool = state $ \raw -> (fst $ head raw, tail raw)

parseConstLengthInt :: Int -> State ParserState Int
parseConstLengthInt len = state $ \raw -> (boolsToInt $ map fst $ take len raw, drop len raw)

parseTriplet :: State ParserState Int
parseTriplet = parseConstLengthInt 3

parseSegment :: State ParserState (Bool, [Bool])
parseSegment = state $ \raw -> ((fst $ head raw, map fst $ take 4 $ tail raw), drop 5 raw)

parseSegments :: State ParserState [Bool]
parseSegments = fmap (concat . map snd) $ unfoldWhileIncludingFinalM fst parseSegment

parseLiteral :: State ParserState Content
parseLiteral = fmap (Literal . boolsToInt) parseSegments

parseOperatorByCount :: Int -> State ParserState Content
parseOperatorByCount times = fmap Operator $ replicateM times parsePacket

current :: State ParserState Int
current = fmap (snd . head) get

reached :: Int -> State ParserState Bool
reached point = fmap (>=point) current

parseOperatorByLength :: Int -> State ParserState Content
parseOperatorByLength len = current >>= \c -> fmap Operator $ untilM parsePacket (reached $ c+len)

parseOperator :: State ParserState Content
parseOperator = do
  bySubpacketsNumber <- parseBool
  if bySubpacketsNumber
    then parseConstLengthInt 11 >>= parseOperatorByCount
    else parseConstLengthInt 15 >>= parseOperatorByLength

parsePacket :: State ParserState Packet
parsePacket = do
  version <- parseTriplet
  identifier <- parseTriplet
  content <- if identifier == 4
    then parseLiteral
    else parseOperator
  return Packet { version = version, identifier = identifier, content = content }

read16 :: Handle -> IO String
read16 = hGetLine
