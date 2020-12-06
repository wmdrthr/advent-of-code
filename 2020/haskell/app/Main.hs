module Main where

import System.Directory    (doesFileExist)
import System.Environment  (getArgs)
import System.TimeIt       (timeIt)
import Text.Printf         (printf)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.Calendar  (toGregorian)
import Data.Time.LocalTime

import AdventOfCode
import ReportRepair       (day01)
import PasswordPhilosophy (day02)
import TobogganTrajectory (day03)
import PassportProcessing (day04)
import BinaryBoarding     (day05)
import CustomCustoms      (day06)

loadFile :: FilePath -> IO String
loadFile filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then readFile filename
    else errorWithoutStackTrace (printf "Input file " ++ filename ++ " not found")

getInput :: Int -> IO String
getInput day = do
  args <- getArgs
  if (length args) > 1 then loadFile (last args)
    else
    loadFile (printf "../inputs/input%02d.txt" day)

guessDay :: IO Int
guessDay = do
  args <- getArgs
  case args of
    [] -> do
      (year, month, day) <- getCurrentTime >>= return . toGregorian . localDay
                            . (utcToLocalTime (TimeZone 330 False "IST"))
      if year /= 2020 || month /= 12 || day > 25
        then errorWithoutStackTrace "AoC 2020 not currently running, day must be provided."
        else return day
    (day:_) -> do
      return (read day)


-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve 1 = day01 . parseInputNumbers
solve 2 = day02 . lines
solve 3 = day03 . lines
solve 4 = day04
solve 5 = day05 . lines
solve 6 = day06
solve n = error (printf "No solver for day %d yet.\n" n)

main :: IO ()
main = do
  day <- guessDay
  input <- getInput day
  timeIt $ solve day (trim input)
