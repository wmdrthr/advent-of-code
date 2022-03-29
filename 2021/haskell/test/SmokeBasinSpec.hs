module SmokeBasinSpec (spec) where

import Test.Hspec
import SmokeBasin as SUT
import AdventOfCode  (parseGrid)

spec :: Spec
spec = do
  let testInput = ["2199943210",
                   "3987894921",
                   "9856789892",
                   "8767896789",
                   "9899965678"]

  let heightMap = parseGrid SUT.parse testInput
  let (lowPoints, riskLevel) = solve1 heightMap
  let basinSize = solve2 heightMap lowPoints
  it "Part 1: risk level => 15" $ do riskLevel `shouldBe` 15
  it "Part 2: basin sizes => 1134" $ do basinSize `shouldBe` 1134
