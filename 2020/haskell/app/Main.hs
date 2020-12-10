module Main where

import System.Directory    (doesFileExist)
import System.Environment  (getArgs)
import System.TimeIt       (timeIt)
import Text.Printf         (printf)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.Calendar  (toGregorian)
import Data.Time.LocalTime
import Control.Applicative (liftA)

import AdventOfCode
import ReportRepair       (day01)
import PasswordPhilosophy (day02)
import TobogganTrajectory (day03)
import PassportProcessing (day04)
import BinaryBoarding     (day05)
import CustomCustoms      (day06)
import HandyHaversacks    (day07)
import HandheldHalting    (day08)
import EncodingError      (day09)
import AdapterArray       (day10)

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


checkUnlocked :: IO Bool
checkUnlocked = do
  let now = getCurrentTime >>= return . localTimeOfDay . (utcToLocalTime (TimeZone 330 False "IST"))
  hours <- (liftA todHour) now
  minutes <- (liftA todMin) now
  return ((hours * 60 + minutes) > 630)


guessDay :: IO Int
guessDay = do
  (year, month, day) <- getCurrentTime >>= return . toGregorian . localDay
                        . (utcToLocalTime (TimeZone 330 False "IST"))
  if year /= 2020 || month /= 12 || day > 25
    then errorWithoutStackTrace "AoC 2020 not currently running, day must be provided."
    else return day


getDay :: IO Int
getDay = do
  args <- getArgs
  case args of
    (day:_) -> return (read day)
    [] -> do
      day <- guessDay
      unlocked <- checkUnlocked
      let currentDay = if unlocked then day else (pred day)
      return currentDay


-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve  1 = day01 . parseInputNumbers
solve  2 = day02 . lines
solve  3 = day03 . lines
solve  4 = day04
solve  5 = day05 . lines
solve  6 = day06
solve  7 = day07 . lines
solve  8 = day08 . lines
solve  9 = day09 . parseInputNumbers
solve 10 = day10 . parseInputNumbers
solve n = error (printf "No solver for day %d yet.\n" n)


main :: IO ()
main = do
  day <- getDay
  input <- getInput day
  timeIt $ solve day (trim input)
