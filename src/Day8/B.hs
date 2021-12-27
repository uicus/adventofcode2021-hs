module Day8.B
     ( solve8B
     ) where

import Data.List
import Data.Function
import Data.Maybe

zero :: ([Int], Int)
zero = ([0, 1, 2, 4, 5, 6], 0)

one :: ([Int], Int)
one = ([2, 5], 1)

two :: ([Int], Int)
two = ([0, 2, 3, 4, 6], 2)

three :: ([Int], Int)
three = ([0, 2, 3, 5, 6], 3)

four :: ([Int], Int)
four = ([1, 2, 3, 5], 4)

five :: ([Int], Int)
five = ([0, 1, 3, 5, 6], 5)

six :: ([Int], Int)
six = ([0, 1, 3, 4, 5, 6], 6)

seven :: ([Int], Int)
seven = ([0, 2, 5], 7)

eight :: ([Int], Int)
eight = ([0, 1, 2, 3, 4, 5, 6], 8)

nine :: ([Int], Int)
nine = ([0, 1, 2, 3, 5, 6], 9)

digitsDecoder :: [([Int], Int)]
digitsDecoder = [zero, one, two, three, four, five, six, seven, eight, nine]

allDigits :: [[Int]]
allDigits = sortBy (compare `on` length) $ map fst digitsDecoder

correspondingInt :: Char -> [(Char, Int)] -> Int
correspondingInt char = snd . fromJust . find ((==char) . fst)

translateFromCurrentCorrespondence :: [(Char, Int)] -> String -> [Int]
translateFromCurrentCorrespondence correspondence = map (flip correspondingInt correspondence)

isContained :: [Int] -> [Int] -> Bool
isContained container = all (flip elem container)

eligibleForWord :: [(Char, Int)] -> String -> Bool
eligibleForWord correspondence word = any (\x -> isContained x translation && isContained translation x) allDigits
  where
    translation = translateFromCurrentCorrespondence correspondence word

eligible :: [(Char, Int)] -> [String] -> Bool
eligible correspondence = all (eligibleForWord correspondence)

allCombinations :: [[(Char, Int)]]
allCombinations = map (flip zip [0..6]) (permutations "abcdefg")

decode :: [Int] -> Int
decode translation = result
  where
    [(_, result)] = filter (\(x, _) -> isContained x translation && isContained translation x) digitsDecoder

intListToInt :: [Int] -> Int
intListToInt = foldl' (\x d -> x*10 + d) 0

decodeSingleEntry :: [String] -> [String] -> Int
decodeSingleEntry words = intListToInt . map (decode . translateFromCurrentCorrespondence properCorrespondence)
  where
    [properCorrespondence] = filter (flip eligible words) allCombinations

solve8B :: [([String], [String])] -> Int
solve8B = foldl' (+) 0 . map (uncurry decodeSingleEntry)
