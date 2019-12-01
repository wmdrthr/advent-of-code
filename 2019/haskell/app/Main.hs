module Main where

import System.Environment (getArgs)
import System.TimeIt (timeIt)
import Text.Printf (printf)

import AdventOfCode

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve n = error (printf "No solver for day %d yet.\n" n)

main :: IO ()
main = do
  day <- guessDay
  input <- getInput day
  timeIt $ solve day input
