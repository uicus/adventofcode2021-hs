module Day20.A
     ( solve20A
     ) where

import Day20.Common

import qualified Data.Map.Strict as Map

solve20A :: (Map.Map Int Bool, Map.Map (Int, Int) Bool) -> Int
solve20A (rules, image) = Map.size $ Map.filter id $ fst $ (!!2) $ iterate (step rules) (image, False)
