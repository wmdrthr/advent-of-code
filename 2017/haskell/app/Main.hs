module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Text.Printf

import AdventOfCode
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)

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
solve 10 = day10

solve n = printf "No solver for day %d yet.%s\n" n

getInput :: Int -> IO String
getInput day = do
  fileExists <- doesFileExist inputFile
  if fileExists
    then readFile inputFile
    else readFile "/dev/null"
  where inputFile = printf "inputs/input%02d.txt" day

main :: IO ()
main = do
  day <- fmap (read . head) getArgs
  input <- getInput day
  solve day input
