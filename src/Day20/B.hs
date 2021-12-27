module Day20.B
     ( solve20B
     ) where

import Day20.Common

import qualified Data.Map.Strict as Map

solve20B :: (Map.Map Int Bool, Map.Map (Int, Int) Bool) -> Int
solve20B (rules, image) = Map.size $ Map.filter id $ fst $ (!!50) $ iterate (step rules) (image, False)
