module Day5.A
     ( solve5A
     ) where

import Day5.Common

eligibleLines :: Line -> Bool
eligibleLines line = horizontal (start line) (end line) || vertical (start line) (end line)

solve5A :: [Line] -> Int
solve5A = countIntersections . filter eligibleLines
