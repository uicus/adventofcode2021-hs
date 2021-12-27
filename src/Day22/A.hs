module Day22.A
     ( solve22A
     ) where

import Day22.Common

interestingCube :: Cube
interestingCube = (Point (-50) (-50) (-50), Point 52 52 52)

solve22A :: [(Switch, Cube)] -> Int
solve22A
  = points
  . filter (flip (inside x) interestingCube . x . fst . snd)
  . filter (flip (inside y) interestingCube . y . fst . snd)
  . filter (flip (inside z) interestingCube . z . fst . snd)
  . filter (flip (inside x) interestingCube . x . snd . snd)
  . filter (flip (inside y) interestingCube . y . snd . snd)
  . filter (flip (inside z) interestingCube . z . snd . snd)
