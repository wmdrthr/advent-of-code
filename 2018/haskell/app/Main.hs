module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.TimeIt
import Text.Printf

import AdventOfCode

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve n _ = printf "No solver for day %d yet.\n" n

getInput :: Int -> IO String
getInput day = do
  fileExists <- doesFileExist inputFile
  if fileExists
    then readFile inputFile
    else readFile "/dev/null"
  where inputFile = printf "../inputs/input%02d.txt" day

run :: IO ()
run = do
  day <- fmap (read . head) getArgs
  input <- getInput day
  timeIt $ solve day input

main :: IO ()
main = do
  timeIt $ run
