module RainRiskSpec where

import Test.Hspec
import RainRisk as SUT

testInput = [ "F10", "N3", "F7", "R90", "F11" ]

spec :: Spec
spec = do

  context "Test Input" $ do
    let actions = map SUT.parse testInput
    it "Part 1 > 25"  $ do SUT.solve12a actions `shouldBe` 25
    it "Part 2 > 286" $ do SUT.solve12b actions `shouldBe` 286
