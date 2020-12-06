module BinaryBoarding (getSeatId, day05) where

import Data.Bool (bool)
import Data.List (sort)

translate :: Char -> Bool
translate 'F' = False
translate 'B' = True
translate 'L' = False
translate 'R' = True

bin2Dec :: [Bool] -> Int
bin2Dec = foldl (\a -> (+) (2*a) . bool 0 1) 0

getSeatId :: String -> Int
getSeatId = bin2Dec . map translate

solve5a :: [String] -> Int
solve5a = maximum . map getSeatId

solve5b :: [String] -> Int
solve5b = findMissing . sort . map getSeatId
  where findMissing (x:y:rest) = if y == succ x
          then findMissing (y:rest)
          else succ x

day05 :: [String] -> IO ()
day05 input = do
  print $ solve5a input
  print $ solve5b input
