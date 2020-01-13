{-# LANGUAGE ViewPatterns #-}

module OrbitalMap (solve1, solve2, parse, day06) where

import           Data.List.Split (splitOn)
import           Data.Functor    ((<&>))
import           Data.Maybe      (fromJust)
import qualified Data.Map        as M
import qualified Data.Set        as S


type OrbitalMap = M.Map String String;

parse:: [String] -> OrbitalMap
parse input = M.fromList [(satellite, primary) | [primary, satellite] <- map (splitOn ")") input]

solve1 :: OrbitalMap -> Int
solve1 orbitalMap = let orbits :: M.Map String Int
                        orbits = orbitalMap <&> \v -> case M.lookup v orbits of
                                                        Nothing -> 1
                                                        Just n  -> n + 1
                    in sum orbits

solve2 :: OrbitalMap -> Int
solve2 orbitalMap = S.size sanTransfers + S.size youTransfers
  where
    sanTransfers = sanOrbits S.\\ youOrbits
    youTransfers = youOrbits S.\\ sanOrbits
    sanOrbits = fromJust $ S.fromList <$> M.lookup "YOU" orbits
    youOrbits = fromJust $ S.fromList <$> M.lookup "SAN" orbits
    orbits = orbitalMap <&> \v -> case M.lookup v orbits of
                                    Nothing -> []
                                    Just s  -> v:s

day06 :: [String] -> IO ()
day06 input = do
  let orbitalMap = parse input
  print $ solve1 orbitalMap
  print $ solve2 orbitalMap
