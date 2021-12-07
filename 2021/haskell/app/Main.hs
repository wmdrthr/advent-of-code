module Main where

import System.Directory       (doesFileExist)
import System.Environment     (getArgs)
import System.CPUTime         (getCPUTime)
import Text.Printf            (printf)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Calendar     (toGregorian)
import Data.Time.LocalTime
import Control.Applicative    (liftA)
import Control.Monad.IO.Class (MonadIO(liftIO))


import AdventOfCode
import SonarSweep          (day01)
import Dive                (day02)
import BinaryDiagnostic    (day03)
import GiantSquid          (day04)
import HydrothermalVenture (day05)
import Lanternfish         (day06)

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
  if year /= 2021 || month /= 12 || day > 25
    then errorWithoutStackTrace "AoC 2021 not currently running, day must be provided."
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


formatElapsedTime :: Integral t => t -> String
formatElapsedTime elapsed = let t :: Double
                                t = fromIntegral elapsed in
                              formatTime t ["ps", "ns", "us", "ms", "s"]
  where formatTime t units
          | t >= 1000.0 = formatTime (t / 1000.0) (tail units)
          | otherwise   = printf "Elapsed: %4.2f %s" t (head units)

-- solve calls the correct solver after parsing the input into the
-- expected format
solve :: Int -> String -> IO ()
solve 1 = day01 . parseInputNumbers
solve 2 = day02 . lines
solve 3 = day03 . lines
solve 4 = day04 . lines
solve 5 = day05 . lines
solve 6 = day06 . parseCommaSeparatedNumbers
solve n = error (printf "No solver for day %d yet.\n" n)


main :: IO ()
main = do
  day <- getDay
  input <- getInput day
  start <- liftIO getCPUTime
  res <- solve day (trim input)
  end <- liftIO getCPUTime
  putStrLn $ formatElapsedTime (end - start)
  return res
