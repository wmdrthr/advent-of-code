module Day14 (
  day14,
  solve1,
  solve2
  ) where

import Data.Word    (Word8)
import Data.Ix      (range)
import Day10        (knotHash)
import Text.Printf  (printf)

import qualified Data.Map     as M

data Square = Square { row :: Int, col :: Int} deriving (Show, Eq, Ord)
type Grid = [String]

grid :: String -> Grid
grid seed = map gridRow [0..127]
  where
    gridRow row = concatMap (printf "%08b") $ knotHash (seed ++ "-" ++ show row)

free :: Grid -> Square -> Bool
free g (Square r c) = (g !! r !! c) == '0'

solve1 :: String -> Int
solve1 = sum . map length . map (filter (== '1')) . grid

dfs :: Grid -> M.Map Square Bool -> Square -> M.Map Square Bool
dfs g m s@(Square r c) | r < 0 || c < 0 ||
                         r > 127 || c > 127 = m
                       | free g s           = m
                       | otherwise          = case M.lookup s m of
                                                Just _  -> m
                                                Nothing -> foldl (dfs g) (M.insert s True m) (neighbors s)
  where
    neighbors (Square r c) = [Square (r + 1) c,
                              Square (r - 1) c,
                              Square r (c + 1),
                              Square r (c - 1)]

groups :: Grid -> (M.Map Square Bool, Int) -> Square -> (M.Map Square Bool, Int)
groups g (m, c) s | free g s  = (m, c)
                  | otherwise = case M.lookup s m of
                             Just _  -> (m, c)
                             Nothing -> ((dfs g m s), c + 1)

solve2 :: String -> Int
solve2 seed = snd $ foldl (groups g) (M.empty, 0) litSquares
  where
    litSquares = filter (not . free g) $ map (uncurry (Square)) $ range ((0,0),(127,127))
    g          = grid seed

day14 :: String -> IO ()
day14 input = do
  print $ solve1 input
  print $ solve2 input
