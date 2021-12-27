module Day4.A
     ( solve4A
     ) where

import Day4.Common

winFastest :: (Int, Int, Board) -> (Int, Int, Board) -> (Int, Int, Board)
winFastest (turn1, number1, board1) (turn2, number2, board2) = if turn1 <= turn2 then (turn1, number1, board1) else (turn2, number2, board2)

solve4A :: ([Int], [Board]) -> Int
solve4A (numbers, boards) = bestResult winFastest numbers boards
