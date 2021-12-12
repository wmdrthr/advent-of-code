module SyntaxScoring where

import Data.List  (elem, elemIndex, sort)
import Data.Maybe (fromJust)
import AdventOfCode

startChars = "{[(<"
endChars   = " )]}>"

match :: Char -> Char
match '{' = '}'
match '[' = ']'
match '(' = ')'
match '<' = '>'
match  _  = error "invalid input"

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score  _  = error "invalid input"

butLast :: [a] -> [a]
butLast l = take (pred $ length l) l

scoreLine :: String -> Int
scoreLine line = step [] line
  where
    step stack []     = foldl calculate 0 (reverse stack)
    step stack (ch:xs)
      | ch `elem` startChars = step (stack ++ [(match ch)]) xs
      | ch /= (last stack)   = (-1) * score ch
      | otherwise            = step (butLast stack) xs
    calculate n ch = (n * 5) + (fromJust (elemIndex ch endChars))

solve1 :: [Int] -> Int
solve1 = abs . sum . filter (<0)

solve2 :: [Int] -> Int
solve2 = median . filter (>0) . sort


day10 :: [String] -> IO ()
day10 lines = do
  let scores = map scoreLine lines
  print $ solve1 scores
  print $ solve2 scores
