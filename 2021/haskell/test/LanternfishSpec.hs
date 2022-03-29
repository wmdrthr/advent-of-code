module LanternfishSpec (spec) where

import Test.Hspec
import Lanternfish as SUT

spec :: Spec
spec = do
  let testInput = SUT.initPopulation [3,4,3,1,2]
  it "Part 1: 80 generations => 5934" $ do SUT.solve testInput 80 `shouldBe` 5934
  it "Part 2: 256 generations => 26984457539" $ do SUT.solve testInput 256 `shouldBe` 26984457539
