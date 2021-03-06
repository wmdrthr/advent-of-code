module Main where

import System.Directory    (doesFileExist)
import System.Environment  (getArgs)
import System.Environment  (getArgs)
import System.TimeIt       (timeIt)
import Text.Printf         (printf)
import Data.List.Split     (splitOn)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.Calendar  (toGregorian)
import Data.Time.LocalTime


import AdventOfCode
import Tsiolkovsky  (day01)
import IntCode      (day02)
import CrossedWires (day03)
import Passwords    (day04)
import OrbitalMap   (day06)

getInput :: Int -> IO String
getInput day = do
  args <- getArgs
  if (length args) > 1 then do return $ last args
    else
    do
      let inputFile = printf "../inputs/input%02d.txt" day
      fileExists <- doesFileExist inputFile
      if fileExists
        then readFile inputFile
        else errorWithoutStackTrace (printf "Input not found for day %d." day)

guessDay :: IO Int
guessDay = do
  args <- getArgs
  case args of
    [] -> do
      (year, month, day) <- getCurrentTime >>= return . toGregorian . localDay
                            . (utcToLocalTime (TimeZone 330 False "IST"))
      if year /= 2019 || month /= 12 || day > 25
        then errorWithoutStackTrace "AoC 2019 not currently running, day must be provided."
        else return day
    (day:_) -> do
      return (read day)


-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve 1 = day01 . lines
solve 2 = day02 . parseCommaSeparatedNumbers
solve 3 = day03 . lines
solve 4 = day04 . map read . splitOn "-"
solve 6 = day06 . lines
solve n = error (printf "No solver for day %d yet.\n" n)

main :: IO ()
main = do
  day <- guessDay
  input <- getInput day
  timeIt $ solve day input
