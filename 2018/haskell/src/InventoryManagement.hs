module InventoryManagement (
  inventoryManagement,
  solve1,
  solve2
  ) where

import AdventOfCode (cardinality, count)

import           Data.Map (Map)
import qualified Data.Map as Map


solve1 :: [String] -> Int
solve1 input = twoCount * threeCount
  where counts = map cardinality input
        twoCount = count (elem 2) counts
        threeCount = count (elem 3) counts

-- | Find the common elements between two strings, which are different
-- in only one position.
--
-- >>> hammingOne "abc" "xyz"
-- Nothing
-- >> hammingOne "abcde" "axcye"
-- Nothing
-- >> hammingOne "fghij" "fguij"
-- Just "fgij"
hammingOne :: String -> String -> Maybe String
hammingOne (x:xs) (y:ys)
  | x == y   = (x :) <$> hammingOne xs ys
  | xs == ys = Just xs
hammingOne _ _ = Nothing

solve2 :: [String] -> String
solve2 input = head [r | x <- input, y <- input, Just r <- [hammingOne x y]]

inventoryManagement :: [String] -> IO ()
inventoryManagement input = do
  print $ solve1 input
  print $ solve2 input
