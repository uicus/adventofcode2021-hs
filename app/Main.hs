module Main where

import Day1.Common
import Day1.A
import Day1.B
import Day2.Common
import Day2.A
import Day2.B
import Day3.Common
import Day3.A
import Day3.B
import Day4.Common
import Day4.A
import Day4.B
import Day5.Common
import Day5.A
import Day5.B
import Day6.Common
import Day6.A
import Day6.B
import Day7.Common
import Day7.A
import Day7.B
import Day8.Common
import Day8.A
import Day8.B
import Day9.Common
import Day9.A
import Day9.B
import Day10.Common
import Day10.A
import Day10.B
import Day11.Common
import Day11.A
import Day11.B
import Day12.Common
import Day12.A
import Day12.B
import Day13.Common
import Day13.A
import Day13.B
import Day14.Common
import Day14.A
import Day14.B
import Day15.Common
import Day15.A
import Day15.B
import Day16.Common
import Day16.A
import Day16.B
import Day17.Common
import Day17.A
import Day17.B
import Day18.Common
import Day18.A
import Day18.B
import Day19.Common
import Day19.A
import Day19.B
import Day20.Common
import Day20.A
import Day20.B
import Day21.Common
import Day21.A
import Day21.B
import Day22.Common
import Day22.A
import Day22.B
import Day23.Common
import Day23.A
import Day23.B
import Day25.Common
import Day25.A
import Common

main :: IO ()
main = do
    day 1  read1  [          "data/testInput1",         "data/input1" ]  [Pr . solve1A,  Pr . solve1B ]
    day 2  read2  [          "data/testInput2",         "data/input2" ]  [Pr . solve2A,  Pr . solve2B ]
    day 3  read3  [          "data/testInput3",         "data/input3" ]  [Pr . solve3A,  Pr . solve3B ]
    day 4  read4  [          "data/testInput4",         "data/input4" ]  [Pr . solve4A,  Pr . solve4B ]
    day 5  read5  [          "data/testInput5",         "data/input5" ]  [Pr . solve5A,  Pr . solve5B ]
    day 6  read6  [          "data/testInput6",         "data/input6" ]  [Pr . solve6A,  Pr . solve6B ]
    day 7  read7  [          "data/testInput7",         "data/input7" ]  [Pr . solve7A,  Pr . solve7B ]
    day 8  read8  (manyNames "data/testInput8_" 2   ++ ["data/input8" ]) [Pr . solve8A,  Pr . solve8B ]
    day 9  read9  [          "data/testInput9",         "data/input9" ]  [Pr . solve9A,  Pr . solve9B ]
    day 10 read10 [          "data/testInput10",        "data/input10"]  [Pr . solve10A, Pr . solve10B]
    day 11 read11 [          "data/testInput11",        "data/input11"]  [Pr . solve11A, Pr . solve11B]
    day 12 read12 (manyNames "data/testInput12_" 3  ++ ["data/input12"]) [Pr . solve12A, Pr . solve12B]
    day 13 read13 [          "data/testInput13",        "data/input13"]  [Pr . solve13A, Pr . solve13B]
    day 14 read14 [          "data/testInput14",        "data/input14"]  [Pr . solve14A, Pr . solve14B]
    day 15 read15 [          "data/testInput15",        "data/input15"]  [Pr . solve15A, Pr . solve15B]
    day 16 read16 (manyNames "data/testInput16_" 12 ++ ["data/input16"]) [Pr . solve16A, Pr . solve16B]
    day 17 read17 [          "data/testInput17",        "data/input17"]  [Pr . solve17A, Pr . solve17B]
    day 18 read18 [          "data/testInput18",        "data/input18"]  [Pr . solve18A, Pr . solve18B]
    day 19 read19 [          "data/testInput19",        "data/input19"]  [Pr . solve19A, Pr . solve19B]
    day 20 read20 [          "data/testInput20",        "data/input20"]  [Pr . solve20A, Pr . solve20B]
    day 21 read21 [          "data/testInput21",        "data/input21"]  [Pr . solve21A, Pr . solve21B]
    day 22 read22 (manyNames "data/testInput22_" 3  ++ ["data/input22"]) [Pr . solve22A, Pr . solve22B]
    day 23 read23 [          "data/testInput23",        "data/input23"]  [Pr . solve23A, Pr . solve23B]
    day 25 read25 [          "data/testInput25",        "data/input25"]  [Pr . solve25A]
