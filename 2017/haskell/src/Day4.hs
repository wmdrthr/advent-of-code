module Day4 (
  day4,
  valid1,
  valid2
  ) where

import qualified Data.Set as Set
import Data.List (sort)

valid1 :: String -> Bool
valid1 passphrase = (length wordList) == (length wordSet)
  where wordList = words passphrase
        wordSet = Set.fromList wordList

valid2 :: String -> Bool
valid2 passphrase = (length wordList) == (length wordSet)
  where wordList = words passphrase
        wordSet = Set.fromList (map sort wordList)

solve1 = length . (filter valid1)
solve2 = length . (filter valid2)


day4 :: [String] -> IO ()
day4 input = do
  print $ solve1 input
  print $ solve2 input
