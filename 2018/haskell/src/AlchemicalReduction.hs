module AlchemicalReduction (
  alchemicalReduction,
  solve1,
  solve2
  ) where

import AdventOfCode
import Data.Char (toLower, toUpper)
import Data.List (nub)

match :: Char -> Char -> Bool
match x y = x /= y && toUpper x == toUpper y

simplify :: String -> String
simplify = foldr step ""
  where
    step x (y:ys) | match x y = ys
    step x ys                 = x : ys

solve1 :: String -> Int
solve1 = length . simplify

solve2 :: String -> Int
solve2 str = minimum lengths
  where str'       = simplify str
        candidates = nub $ map toLower str'
        isOk bad x = bad /= toLower x
        lengths    = [solve1 (filter (isOk bad) str') | bad <- candidates]

alchemicalReduction :: String -> IO ()
alchemicalReduction input = do
  print $ solve1 input
  print $ solve2 input
