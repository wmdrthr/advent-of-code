module InventoryManagement (
  inventoryManagement,
  solve1,
  solve2
  ) where

import AdventOfCode

import           Data.Map (Map)
import qualified Data.Map as Map

-- | Build a map with the cardinalities of each letter in the given
-- String
--
  -- >>> cardinality "foobar"
  -- fromList [('a',1),('b',1),('f',1),('o',2),('r',1)]
cardinalities :: Ord a => [a] -> Map a Int
cardinalities xs = Map.fromListWith (+) [ (x,1) | x <- xs]

-- | count number of items that satisfy the predicate
count :: Ord a => (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

solve1 :: [String] -> Int
solve1 input = twoCount * threeCount
  where inputCardinalities = map cardinalities input
        twoCount = count (elem 2) inputCardinalities
        threeCount = count (elem 3) inputCardinalities

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
