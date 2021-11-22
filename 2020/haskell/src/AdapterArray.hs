module AdapterArray where

import qualified Data.Set as S
import Data.List          (findIndices)
import Data.Function      (fix)

nextAdapter :: S.Set Int -> Int -> [Int]
nextAdapter ratings current = [(current + difference) | difference <- [1,2,3],
                                (current + difference) `S.member` ratings]

differences :: S.Set Int -> Int -> Int
differences ratings maxRating = go 0 []
  where go current acc = if current >= maxRating
                         then result $ acc ++ [3]
                         else let next = head $ nextAdapter ratings current in
                                go next (acc ++ [(next - current)])
        result acc = ((length . findIndices (==1)) acc) * ((length . findIndices (==3)) acc)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

countAdapterChains :: S.Set Int -> Int -> Int
countAdapterChains ratings maxRating = mgo 0
  where go f current
          | current == maxRating = 1
          | otherwise            = sum $ map f (nextAdapter ratings current)
        mgo = fix (memoize . go)

day10 :: [Int] -> IO ()
day10 input = do
  let ratings = S.fromList input
  let maxRating = maximum input
  print $ differences ratings maxRating
  print $ countAdapterChains ratings maxRating
