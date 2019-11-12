module Day10 (
  day10,
  solve1,
  solve2
  ) where

import AdventOfCode         (parseCommaSeparatedNumbers)
import Data.Word            (Word8)
import Data.Bits            (xor)
import Data.Char            (ord)
import Data.List.Split      (splitOn, chunksOf)
import Data.List            as L
import Data.Vector.Storable as V
import Text.Printf          (printf)

data HashState = HS { hsVec  :: V.Vector Word8
                    , hsPos  :: Word8
                    , hsSkip :: Word8
                    }

step :: HashState -> Word8 -> HashState
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    indexes = fromIntegral . (+ p0) <$> L.init [0..n]
    vals = (v0 V.!) <$> indexes
    v1 = v0 V.// zip indexes (L.reverse vals)
    p1 = p0 + n + s0
    s1 = s0 + 1

process :: [Word8] -> V.Vector Word8
process = hsVec . L.foldl' step hs0
  where
    hs0 = HS (V.generate 256 fromIntegral) 0 0

solve1 :: String -> Int
solve1 = V.product . V.map fromEnum . V.take 2 . process
         . L.map toEnum . parseCommaSeparatedNumbers

knotHash :: String -> [Word8]
knotHash = L.map (L.foldr xor 0) . chunksOf 16 . V.toList . process
           . L.concat . L.replicate 64 . (L.++ salt)
           . L.map (fromIntegral . ord)
  where
    salt = [17, 31, 73, 47, 23]

solve2 :: String -> String
solve2 = L.concatMap (printf "%02x") . knotHash

day10 :: String -> IO ()
day10 input = do
  print $ solve1 input
  print $ solve2 input
