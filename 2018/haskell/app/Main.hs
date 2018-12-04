module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.TimeIt
import Text.Printf

import AdventOfCode

import ChronalCalibration (chronalCalibration)
import InventoryManagement (inventoryManagement)
import FabricSlices (fabricSlices)

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve 1 = chronalCalibration . parseInputNumbersWithSign
solve 2 = inventoryManagement . lines
solve 3 = fabricSlices . lines
solve n = printf "No solver for day %d yet.%s\n" n

getInput :: Int -> IO String
getInput day = do
  fileExists <- doesFileExist inputFile
  if fileExists
    then readFile inputFile
    else readFile "/dev/null"
  where inputFile = printf "../inputs/input%02d.txt" day

main :: IO ()
main = do
  day <- fmap (read . head) getArgs
  input <- getInput day
  timeIt $ solve day input
