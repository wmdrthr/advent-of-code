module AdventOfCode (
  parseInputSimple,
  parseInputNumbers,
  parseInputNumbersWithSign,
  parseInputNumberLists,
  parseCommaSeparatedNumbers,
  cardinality,
  count,
  maxKey,
  getInput,
  guessDay
  ) where

import Text.Printf
import System.Directory   (doesFileExist)
import System.Environment (getArgs)
import Data.Map           (Map)
import Data.List          (maximumBy)
import Data.List.Split    (splitOn)
import Data.Ord           (comparing)

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime


import qualified Data.Map as Map

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

toDigits :: Integer -> [Int]
toDigits number
  | number == 0 = []
  | otherwise = (toDigits quotient) ++ [remainder]
  where
    remainder = fromIntegral (number `mod` 10) :: Int
    quotient = number `div` 10

-- | Convert a String containing a number into a list of its
-- individual digits.
--
-- >>> parseInputSimple "1212"
-- [1, 2, 1, 2]
parseInputSimple :: String -> [Int]
parseInputSimple string = toDigits (read string :: Integer)

-- | Parse a String containing a number on each line into a list of
-- numbers.
--
-- >>> parseInputNumbers "12\n34\n56"
--- [12, 34, 56]
parseInputNumbers :: String -> [Int]
parseInputNumbers = (map read) . lines

-- | Parse a String containing a number on each line into a list of
-- numbers; each number has either a '+' or a '-' sign.
--
-- >>> parseInputNumbersWithSign "+12\n-34\n+56"
-- [12, -34, 56]
parseInputNumbersWithSign :: String -> [Int]
parseInputNumbersWithSign = (map readInt) . lines
  where readInt ('+':xs) = read xs
        readInt xs       = read xs

-- | Parse a String containing multiple numbers on each line into a
-- list of list of numbers.
--
-- >>> parseInputNumberList" 1\t2\t3\n4\t5\t6"
-- [[1, 2, 3], [4, 5, 6]]
parseInputNumberLists :: String -> [[Int]]
parseInputNumberLists string = map (map read) $ map words $ lines string

-- | Parse a comma separated list of numbers e.g. "1,2,3" -> [1,2,3]
parseCommaSeparatedNumbers :: String -> [Int]
parseCommaSeparatedNumbers = (map read) . (splitOn ",")

-- | Calclate the number of occurences of each item in a given list.
--
-- >>> cardinality "foobar"
-- fromList [('a',1),('b',1),('f',1),('o',2),('r',1)]
cardinality :: Ord a => [a] -> Map a Int
cardinality xs = Map.fromListWith (+) [ (x,1) | x <- xs]

-- | Count the number of elements in a list (or any foldable) matching a predicate.
--
-- >>> count even [1 .. 10]
-- 5
-- >>> count (> 1) (cardinality "foobar")
-- 1
count :: Foldable t => (a -> Bool) -> t a -> Int
count pred = foldl (\acc x -> if pred x then acc + 1 else acc) 0


-- | Find the key with the largest value in a Map
maxKey :: Ord a => Map k a -> k
maxKey = fst . maximumBy (comparing snd) . Map.toList
