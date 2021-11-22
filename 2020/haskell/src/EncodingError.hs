module EncodingError where

import           AdventOfCode
import qualified Data.Set     as S
import Data.List              (sort)

slice :: [Int] -> Int -> Int -> [Int]
slice ns start end = take (end-start) $ drop start ns

calculateSums :: [Int] -> S.Set Int
calculateSums numbers = S.fromList (map sum (combinations 2 numbers))

solve9a :: [Int] -> Int -> Int
solve9a numbers windowSize =
  let sums = calculateSums (slice numbers 0 windowSize) in
    if (numbers !! windowSize) `S.member` sums
    then solve9a (tail numbers) windowSize
    else (numbers !! windowSize)

solve9b :: [Int] -> Int -> Int
solve9b numbers target = calculate 0 1
  where calculate start end
          | target == rangeSum start end = result start end
          | target >  rangeSum start end = calculate start (succ end)
          | target <  rangeSum start end = calculate (succ start) end
        result a b = let ns = sort $ slice numbers a b in
                             (head ns) + (last ns)
        rangeSum a b = sum $ slice numbers a b

day09 :: [Int] -> IO ()
day09 input = do
  let target = solve9a input 25
  print $ target
  print $ solve9b input target
