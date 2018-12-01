module AdventOfCode (
  parseInputSimple,
  parseInputNumbers,
  parseInputNumbersWithSign,
  parseInputNumberLists
  ) where

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

-- parse a String containing a number on each line into a list of
-- numbers; each number has either a '+' or a '-' sign
-- e.g. "+12\n-34\n+56" -> [12, -34, 56]
parseInputNumbersWithSign :: String -> [Int]
parseInputNumbersWithSign = (map readInt) . lines
  where readInt ('+':xs) = read xs
        readInt xs       = read xs

-- parse a String containing multiple numbers on each line into a list
-- of list of numbers e.g. "1\t2\t3\n4\t5\t6" -> [ [1, 2, 3], [4, 5, 6]]
parseInputNumberLists :: String -> [[Int]]
parseInputNumberLists string = map (map read) $ map words $ lines string
