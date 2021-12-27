module Day23.A
     ( solve23A
     ) where

import Day23.Common

solve23A :: [Sideroom] -> Int
solve23A = getMinResult . start
