module CalorieCounting where

import Data.List.Split (splitOn)
import Data.List       (sort)

parseFood :: String -> [Int]
parseFood input = map sum $ map (map read) $ map (splitOn "\n") $ splitOn "\n\n" input

solve1 :: [Int] -> Int
solve1 = maximum

solve2 :: [Int] -> Int
solve2 = sum . (take 3) . reverse . sort


day01 :: String -> IO ()
day01 input = do
    let food = parseFood input
    print $ solve1 food
    print $ solve2 food