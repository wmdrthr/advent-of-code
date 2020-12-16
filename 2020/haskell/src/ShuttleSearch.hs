module ShuttleSearch where

import Data.List.Split (splitOn)
import Data.Maybe      (mapMaybe)

parse :: String -> [(Integer, Integer)]
parse schedule = map (\(i,b) -> (i, read b :: Integer))
                 $ filter (\(_, b) -> if b == "x" then False else True)
                 $ zip [0..] (splitOn "," schedule)

powmod :: Integer -> Integer -> Integer -> Integer
powmod base exp modulus = powmod' base exp modulus 1
  where powmod' _ 0 _ r = r
        powmod' b e m r
          | odd e     = powmod' ((b * b) `mod` m) (e `div` 2) m ((r * b) `mod` m)
          | otherwise = powmod' ((b * b) `mod` m) (e `div` 2) m r

solve13a :: Integer -> [(Integer, Integer)] -> Integer
solve13a delay buses = head $ mapMaybe check [delay..]
  where
    check :: Integer -> Maybe Integer
    check ts = let values = mapMaybe (calc ts) buses in
                 if length values > 0
                 then Just (head values)
                 else Nothing
    calc ts (_,bus) = if (ts + 1) `mod` bus == 0
                      then Just (((ts + 1) - delay) * bus)
                      else Nothing

solve13b :: [(Integer, Integer)] -> Integer
solve13b buses = total `mod` product'
  where total = sum $ map calc buses
        calc (idx, bus) = let d = product' `div` bus in
                            (bus - idx) * d * (powmod d (bus - 2) bus)
        product' = product $ map snd buses

day13 :: [String] -> IO ()
day13 input = do
  let delay = read (head input) :: Integer
  let buses = parse (last input)
  print $ solve13a delay buses
  print $ solve13b buses
