module SonarSweepSpec (spec) where

import Test.Hspec
import SonarSweep as SUT

spec :: Spec
spec = do
  let testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  context "Part 1" $ do
    it "increasing values => 7" $ do SUT.calculate testInput `shouldBe` 7
  context "Part 2" $ do
    it "sliding window increasing values => 5" $ do SUT.calculate (SUT.sums testInput) `shouldBe` 5
