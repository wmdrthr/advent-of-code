module Main where

import System.Environment (getArgs)
import Text.Printf        (printf)

import AdventOfCode
import Day01 (day1)
import Day02 (day2)
import Day03 (day3)
import Day04 (day4)
import Day05 (day5)
import Day06 (day6)
import Day07 (day7)
import Day08 (day8)
import Day09 (day9)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve  1 = day1 . parseInputSimple
solve  2 = day2 . parseInputNumberLists
solve  3 = day3 . read
solve  4 = day4 . lines
solve  5 = day5 . parseInputNumbers
solve  6 = day6 . head . parseInputNumberLists
solve  7 = day7 . lines
solve  8 = day8 . lines
solve  9 = day9
solve 10 = day10 . strip
solve 11 = day11 . strip
solve 12 = day12 . lines
solve 13 = day13 . lines
solve 14 = day14 . strip
solve 15 = day15 . lines
solve n  = printf "No solver for day %d yet.%s\n" n

main :: IO ()
main = do
  day <- fmap (read . head) getArgs
  input <- getTestInput day
  solve day input
