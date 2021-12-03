module BinaryDiagnostic where

import qualified Data.Map.Strict as Map
import           AdventOfCode    (binS2Dec)

invertBit :: Char -> Char
invertBit '0' = '1'
invertBit '1' = '0'
invertBit  x  =  x

mostCommonBit :: [String] -> Int -> Char
mostCommonBit numbers column = let counter = Map.fromListWith (+) (map (\n -> (n !! column, 1)) numbers) in
                                 if (counter Map.! '1') >= (counter Map.! '0') then '1' else '0'

leastCommonBit :: [String] -> Int -> Char
leastCommonBit numbers column = invertBit $ mostCommonBit numbers column


solve1 :: [String] -> Int
solve1 numbers = gamma * epsilon
  where gamma   = binS2Dec $ calculate mostCommonBit
        epsilon = binS2Dec $ calculate leastCommonBit
        calculate bitFn = foldl (\num c -> num ++ [(bitFn numbers c)]) "" [0..((pred . length . head) numbers)]

filterNumbers :: ([String] -> Int -> Char) -> [String] -> Int -> String
filterNumbers _ (n:[]) _ = n
filterNumbers bitFn numbers column  = filterNumbers bitFn filteredNumbers (column + 1)
  where filteredNumbers = filter (\n -> (n !! column) == filterChar) numbers
        filterChar      = bitFn numbers column

rating :: ([String] -> Int -> Char) -> [String] -> Int
rating bitFn numbers = binS2Dec $ (filterNumbers bitFn numbers 0)

solve2 :: [String] -> Int
solve2 numbers = oxygen * carbondioxide
  where oxygen        = rating mostCommonBit numbers
        carbondioxide = rating leastCommonBit numbers


day03 :: [String] -> IO ()
day03 input = do
  print $ solve1 input
  print $ solve2 input
