module CustomCustoms where

import qualified Data.Map        as M
import           Data.List.Split (splitOn)

processResponse :: M.Map Char Int -> String -> M.Map Char Int
processResponse m r = M.unionWith (+) m (M.fromList (zip r (repeat 1)))

collectAnswers ::  [String] -> M.Map Char Int
collectAnswers responses = foldl processResponse M.empty responses

solve6a :: [[String]] -> Int
solve6a = sum . map (length . M.keys . collectAnswers)

solve6b :: [[String]] -> Int
solve6b groups = sum $ map processGroup groups
  where processGroup group = (length . M.keys . M.filter (== (length group)) . collectAnswers) group

day06 :: String -> IO ()
day06 input = do
  let groups = map (splitOn "\n") $ splitOn "\n\n" input
  print $ solve6a groups
  print $ solve6b groups
