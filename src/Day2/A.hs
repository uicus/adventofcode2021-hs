module Day2.A
     ( solve2A
     ) where

import Day2.Common

import Data.List

data Position = Position { x :: !Int, y :: !Int }

multiplyCoordinates :: Position -> Int
multiplyCoordinates position = x position * y position

applyCommand :: Position -> Command -> Position
applyCommand position command =
  case command of
    Forward dx -> position { x = x position + dx }
    Up dy -> position { y = y position - dy }
    Down dy -> position { y = y position + dy }

solve2A :: [Command] -> Int
solve2A = multiplyCoordinates . foldl' applyCommand (Position { x = 0, y = 0 })
