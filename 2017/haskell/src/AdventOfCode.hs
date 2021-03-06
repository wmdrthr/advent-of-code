module AdventOfCode (
  getTestInput,
  parseInputSimple,
  parseInputNumbers,
  parseInputNumberLists,
  parseCommaSeparatedNumbers,
  strip
  ) where

import Data.List.Split  (splitOn)
import System.Directory (doesFileExist)
import Text.Printf      (printf)
import qualified Data.Text as T

getTestInput :: Int -> IO String
getTestInput day = do
  fileExists <- doesFileExist inputFile
  if fileExists
    then readFile inputFile
    else readFile "/dev/null"
  where inputFile = printf "inputs/input%02d.txt" day

strip :: String -> String
strip = T.unpack . T.strip . T.pack

toDigits :: Integer -> [Int]
toDigits number
  | number == 0 = []
  | otherwise = (toDigits quotient) ++ [remainder]
  where
    remainder = fromIntegral (number `mod` 10) :: Int
    quotient = number `div` 10

-- parse a String into individual digits
-- "1212" -> [1, 2, 1, 2]
parseInputSimple :: String -> [Int]
parseInputSimple string = toDigits (read string :: Integer)

-- parse a String containing a number on each line into a list of
-- numbers e.g. "12\n34\n56" -> [12, 34, 56]
parseInputNumbers :: String -> [Int]
parseInputNumbers = (map read) . lines

-- parse a comma separated list of numbers e.g. "1,2,3" -> [1,2,3]
parseCommaSeparatedNumbers :: String -> [Int]
parseCommaSeparatedNumbers = (map read) . (splitOn ",")

-- parse a String containing multiple numbers on each line into a list
-- of list of numbers e.g. "1\t2\t3\n4\t5\t6" -> [ [1, 2, 3], [4, 5, 6]]
parseInputNumberLists :: String -> [[Int]]
parseInputNumberLists string = map (map read) $ map words $ lines string
