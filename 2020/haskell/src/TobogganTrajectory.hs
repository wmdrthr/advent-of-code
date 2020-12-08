module TobogganTrajectory where

import           Prelude  hiding (traverse)

treeAt :: [String] -> (Int, Int) -> Int
treeAt trees (x, y) = fromEnum $ cycle row !! x == '#'
  where row = trees !! y

traverse :: [String] -> (Int, Int) -> Int
traverse trees (dx, dy) = sum $ map (treeAt trees) (zip xs ys)
  where xs = [0,dx..]
        ys = [0,dy..(length trees - 1)]

solve3a :: [String] -> Int
solve3a trees = traverse trees (3, 1)

solve3b :: [String] -> Int
solve3b trees = product $ map (traverse trees) [(1,1), (3,1), (5,1), (7,1), (1,2)]

day03 :: [String] -> IO ()
day03 input = do
  print $ solve3a input
  print $ solve3b input
