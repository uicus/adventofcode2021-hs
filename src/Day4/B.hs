module Day4.B
     ( solve4B
     ) where

import Day4.Common

winLatest :: (Int, Int, Board) -> (Int, Int, Board) -> (Int, Int, Board)
winLatest (turn1, number1, board1) (turn2, number2, board2) = if turn1 >= turn2 then (turn1, number1, board1) else (turn2, number2, board2)

solve4B :: ([Int], [Board]) -> Int
solve4B (numbers, boards) = bestResult winLatest numbers boards
