module Day2.B
     ( solve2B
     ) where

import Day2.Common

import Data.List

data Position = Position { x :: !Int, y :: !Int, aim :: !Int }

multiplyCoordinates :: Position -> Int
multiplyCoordinates position = x position * y position

applyCommand :: Position -> Command -> Position
applyCommand position command =
  case command of
     Forward dx -> position { x = x position + dx, y = y position + aim position * dx }
     Up dy -> position { aim = aim position - dy }
     Down dy -> position { aim = aim position + dy }

solve2B :: [Command] -> Int
solve2B = multiplyCoordinates . foldl' applyCommand (Position { x = 0, y = 0, aim = 0})
