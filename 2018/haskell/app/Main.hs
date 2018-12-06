module Main where

import System.Environment (getArgs)
import System.TimeIt (timeIt)
import Text.Printf (printf)

import AdventOfCode

import ChronalCalibration (chronalCalibration)
import InventoryManagement (inventoryManagement)
import FabricSlices (fabricSlices)
import SleepingGuards (sleepingGuards)

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve 1 = chronalCalibration . parseInputNumbersWithSign
solve 2 = inventoryManagement . lines
solve 3 = fabricSlices . lines
solve 4 = sleepingGuards . lines
solve n = error (printf "No solver for day %d yet.\n" n)

main :: IO ()
main = do
  day <- fmap (read . head) getArgs
  input <- getInput day
  timeIt $ solve day input
