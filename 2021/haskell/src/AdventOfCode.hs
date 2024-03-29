{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module AdventOfCode where

import Data.List        (maximumBy, dropWhileEnd)
import Data.List.Split  (splitOn)
import Data.Ord         (comparing)
import Data.Char        (isSpace)
import Data.Bool        (bool)
import Data.Maybe       (mapMaybe)
import Prelude   hiding (Left, Right)

import qualified Data.Map.Strict as M


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
cardinality :: Ord a => [a] -> M.Map a Int
cardinality xs = M.fromListWith (+) [ (x,1) | x <- xs]

-- | Count the number of elements in a list (or any foldable) matching a predicate.
--
-- >>> count even [1 .. 10]
-- 5
-- >>> count (> 1) (cardinality "foobar")
-- 1
count :: Foldable t => (a -> Bool) -> t a -> Int
count pred = foldl (\acc x -> if pred x then acc + 1 else acc) 0


-- | Find the key with the largest value in a Map
maxKey :: Ord a => M.Map k a -> k
maxKey = fst . maximumBy (comparing snd) . M.toList


-- Manhattan Distance between two points
manhattanDistance :: Num a => (a,a) -> (a,a) -> a
manhattanDistance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

-- | Generate all unique combinations of length k from the given sequence
--
-- >>> combinations 2 [0..2]
-- [[0,1],[0,2],[1,2]]
combinations :: Int -> [a] -> [[a]]
combinations 1 as        = map pure as
combinations k as@(_:xs) = run (l-1) (k-1) as $ combinations (k-1) xs
                             where
                             l = length as

                             run :: Int -> Int -> [a] -> [[a]] -> [[a]]
                             run n k ys cs | n == k    = map (ys ++) cs
                                           | otherwise = map (q:) cs ++ run (n-1) k qs (drop dc cs)
                                           where
                                           (q:qs) = take (n-k+1) ys
                                           dc     = product [(n-k+1)..(n-1)] `div` product [1..(k-1)]


-- | Exclusive-or for booleans
-- >>> True `xor` False
-- True
-- >>> True `xor` True
-- False
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False


-- | Substring
-- >>> substring 5 9 "This is a test"
-- 'is a'
substring :: Int -> Int -> String -> String
substring start end string = take (end - start) (drop start string)


-- | Slice of list
-- >>> slice 2 7 [0, 1, 2, 3, 4, 5,6, 7, 8, 9, 10]
-- [2, 3, 4, 5, 6]
slice begin end = take (end - begin) . drop begin


-- | Remove whitespace from beginning and end of string
-- >>> trim "  foo\n"
-- "foo"
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


-- Convert binary to decimal
-- >>> bin2Dec [True, False, True, True, False]
-- 22
bin2Dec :: [Bool] -> Int
bin2Dec = foldl (\a -> (+) (2*a) . bool 0 1) 0


-- Convert binary string to decimal
-- >>> binS2Dec "01001"
-- 9
binS2Dec :: String -> Int
binS2Dec = bin2Dec . map (=='1')


-- Median (middle value) of numeric data, using the common “mean of
-- middle two” method (and using integer division)
-- >>> median [1, 3, 5]
-- 3
-- >>> median [1, 3, 5, 7]
-- 4
median :: Integral a => [a] -> a
median xs
  | odd  len = xs !! mid
  | even len = (xs !! (mid-1) + xs !! mid) `quot` 2
    where len = length xs
          mid = len `div` 2

-- Below functions assume origin is at top-left,
-- with (X,Y) -> X = row, # Y = column.

type V2 = (Int, Int)

data Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight

data Grid a = Grid { points :: M.Map V2 a,
                     rows :: Int,
                     cols :: Int
                   } deriving (Show)

parseGrid :: (Char -> a) -> [String] -> Grid a
parseGrid parse lines = Grid grid rows cols
  where grid = foldl parseLine M.empty (zip [0..] lines)
        parseLine m (row, line) = foldl (\m (col, val) -> M.insert (row, col) (parse val) m) m (zip [0..] line)
        rows = length lines
        cols = length (head lines)

neighbor :: Grid a -> V2 -> Direction -> Maybe V2
neighbor _          (x,y) Up        = if x > 0         then Just (x - 1, y) else Nothing
neighbor Grid{rows} (x,y) Down      = if x + 1 < rows  then Just (x + 1, y) else Nothing
neighbor _          (x,y) Left      = if y > 0         then Just (x, y - 1) else Nothing
neighbor Grid{cols} (x,y) Right     = if y + 1 < cols  then Just (x, y + 1) else Nothing
neighbor _          (x,y) UpLeft    = if x > 0 && y > 0        then Just (x - 1, y - 1) else Nothing
neighbor Grid{cols} (x,y) UpRight   = if x > 0 && y + 1 < cols then Just (x - 1, y + 1) else Nothing
neighbor Grid{rows} (x,y) DownLeft  = if x + 1 < rows && y > 0 then Just (x + 1, y - 1) else Nothing
neighbor grid       (x,y) DownRight = if x + 1 < (rows grid) && y + 1 < (cols grid)
                                       then Just (x + 1, y + 1)
                                       else Nothing

neighbors :: Grid a -> V2 -> [V2]
neighbors grid point = mapMaybe (neighbor grid point) [Up, Down, Left, Right]

diagonalNeighbors :: Grid a -> V2 -> [V2]
diagonalNeighbors grid point = mapMaybe (neighbor grid point) [UpLeft, UpRight, DownLeft, DownRight]

allNeighbors :: Grid a -> V2 -> [V2]
allNeighbors grid point = (neighbors grid point) ++ (diagonalNeighbors grid point)
