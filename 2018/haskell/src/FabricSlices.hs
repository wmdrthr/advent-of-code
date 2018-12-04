module FabricSlices (
  fabricSlices,
  solve1,
  solve2,
  parseClaim,
  cutFabric
  ) where

import           AdventOfCode (cardinality, count)
import           Text.Regex.Posix
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Claim = Claim { claimId :: Int
                   , startx  :: Int
                   , starty  :: Int
                   , width   :: Int
                   , height  :: Int
                   } deriving Show

type Fabric = Map (Int,Int) Int

parseClaim :: String -> Claim
parseClaim s = go $ map (read . head) matches
  where matches = s =~ "[0-9]+" :: [[String]]
        go (a:b:c:d:e:_) = Claim a b c d e

-- | Given a claim with a starting position (x,y) and a width and
-- height, return all the points covered by this claim as a list of
-- points
-- >>> resolveClaim (Claim 123 1 1 2 2)
-- [(1,1),(1,2),(2,1),(2,2)]
resolveClaim :: Claim -> [(Int,Int)]
resolveClaim claim = [ (x,y)
                     | x <- [startx claim .. startx claim + width claim - 1]
                     , y <- [starty claim .. starty claim + height claim -1]
                     ]
-- | Given a list of claims, return a map of all points covered by the
-- claims, along with how many claims apply to each point.
cutFabric :: [Claim] -> Fabric
cutFabric = cardinality . concatMap resolveClaim

solve1 :: Fabric -> Int
solve1 = count (> 1)

solve2 :: Fabric -> [Claim] -> Int
solve2 fabric claims = head [ claimId claim
                            | claim <- claims
                            , all (1 ==) (Map.intersection fabric (cutFabric [claim]))
                            ]

fabricSlices :: [String] -> IO ()
fabricSlices input = do
  print $ solve1 fabric
  print $ solve2 fabric claims
    where claims = map parseClaim input
          fabric = cutFabric claims
