module Lanternfish where

import qualified Data.Map as M

initPopulation :: [Int] -> M.Map Int Int
initPopulation initial = foldl f M.empty initial
  where f population fish = M.insertWith (+) fish 1 population

generation :: M.Map Int Int -> M.Map Int Int
generation population = foldl next M.empty (M.toList population)
  where next pop (age, count)
          | age > 0   = M.insertWith (+) (age - 1) count pop
          | otherwise = M.insertWith (+) 6 count (M.insertWith (+) 8 count pop)

solve :: M.Map Int Int -> Int -> Int
solve lanternfish days = sum $ map snd . M.toList $ last $ take (days + 1) $ iterate generation lanternfish

day06 :: [Int] -> IO ()
day06 input = do
  let lanternfish = initPopulation input
  print $ solve lanternfish 80
  print $ solve lanternfish 256
