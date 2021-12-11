module SevenSegmentSearch where

import qualified Data.Map        as M
import           Data.List.Split (splitOn)

canonicalPattern = "abcefg cf acdeg acdfg bdcf abdfg abdefg acf abcdefg abcdfg" :: String

solve1 :: [String] -> Int
solve1 = sum . map process
  where process = length . filter (\n -> n `elem` [2, 3, 4, 7]) . map length . words

score :: String -> M.Map Char Int
score chars = foldl (\m c -> M.insertWith (+) c 1 m) M.empty $ filter (/=' ') chars

lookupTable :: M.Map Int Int
lookupTable = foldl (\m (digit, signals) -> M.insert (process signals) digit m) M.empty (zip [0..] (words canonicalPattern))
  where process          = sum . map (\c -> canonicalScores M.! c)
        canonicalScores  = score canonicalPattern

toNumber :: [String] -> Int
toNumber (signals:outputs:[]) = collect 0 $ map outputWordToDigit (words outputs)
  where outputWordToDigit outputWord = lookupTable M.! (digitScore outputWord)
        digitScore  = sum . map (\c -> signalScore M.! c)
        signalScore = score signals
        collect n (d:ds) = collect (n * 10 + d) ds
        collect n []     = n

solve2 :: [[String]] -> Int
solve2 = sum . map toNumber


day08 :: [String] -> IO ()
day08 input = do
  let lines = map (splitOn " | ") input
  print $ solve1 (map (!!1) lines)
  print $ solve2 lines
