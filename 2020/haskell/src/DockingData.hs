{-# LANGUAGE NamedFieldPuns #-}

module DockingData where

import qualified Data.Map.Strict as M
import           Data.List        (isPrefixOf)
import           AdventOfCode
import           Data.Char        (isDigit)
import           Data.Bits

data Line = Mask { andMask :: Int, orMask  :: Int, maskStr :: String }
          | Instr { address :: Int, value :: Int }
          deriving (Show)

parseBinary ::  Bool -> String -> Int
parseBinary rep = bin2Dec . map translate
  where translate '0' = False
        translate '1' = True
        translate 'X' = rep

parse :: String -> Line
parse line
  | "mask" `isPrefixOf` line = parseMask line
  | otherwise                = parseInstr line
  where parseMask line = let maskStr = drop 7 line in
                           Mask (parseBinary True maskStr) (parseBinary False maskStr) maskStr
        parseInstr line = let (addr:_:value:[]) = words line in
                            Instr (read $ takeWhile (\c -> isDigit c) $ drop 4 addr) (read value)

solve14a :: [Line] -> Int
solve14a program = M.foldl (+) 0 $ go M.empty (0, 0) program
  where go :: M.Map Int Int -> (Int, Int) -> [Line] -> M.Map Int Int
        go memory _     []      = memory
        go memory masks (x:xs) = case x of
                                   Mask a o _       -> go memory (a,o) xs
                                   Instr addr value -> go (M.insert addr (applyMasks masks value) memory) masks xs
        applyMasks (andMask, orMask) value = (value .|. orMask) .&. andMask

floatingAddresses :: String -> Int -> [Int]
floatingAddresses maskStr pos = go maskStr pos []
  where
    go :: String -> Int -> [Int] -> [Int]
    go []      _   acc = acc ++ [0]
    go maskStr pos acc
          | (last maskStr) == '0' = acc ++ (map (\a -> 2 * a + pos `mod` 2) $ go (subs maskStr) (pos `div` 2) acc)
          | (last maskStr) == '1' = acc ++ (map (\a -> 2 * a + 1) $ go (subs maskStr) (pos `div` 2) acc)
          | (last maskStr) == 'X' = acc ++ (concat (map (\a -> [2 * a + 0, 2 * a + 1]) $ go (subs maskStr) (pos `div` 2) acc))
    subs s = substring 0 (pred $ length s) s

solve14b :: [Line] -> Int
solve14b program = M.foldl (+) 0 $ go M.empty "" program
  where go :: M.Map Int Int -> String -> [Line] -> M.Map Int Int
        go memory _ [] =  memory
        go memory mask (x:xs) = case x of
                                  Mask  _ _  ms    -> go memory ms xs
                                  Instr addr value -> go (insertAll memory $ (zip (floatingAddresses mask addr) (repeat value))) mask xs
        insertAll :: M.Map Int Int -> [(Int, Int)] -> M.Map Int Int
        insertAll m xs = M.union (M.fromList xs) m

day14 :: [String] -> IO ()
day14 input = do
  let program = map parse input
  print $ solve14a program
  print $ solve14b program
