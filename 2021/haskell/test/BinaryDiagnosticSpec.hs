module BinaryDiagnosticSpec (spec) where

import Test.Hspec
import BinaryDiagnostic as SUT

spec :: Spec
spec = do
  let testInput = ["00100",
                   "11110",
                   "10110",
                   "10111",
                   "10101",
                   "01111",
                   "00111",
                   "11100",
                   "10000",
                   "11001",
                   "00010",
                   "01010"]
  it "Part 1: power consumption => 198" $ do SUT.solve1 testInput `shouldBe` 198
  it "Part 2: life support rating => 230" $ do SUT.solve2 testInput `shouldBe` 230
