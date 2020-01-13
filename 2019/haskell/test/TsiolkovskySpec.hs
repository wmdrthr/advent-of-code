module TsiolkovskySpec (spec) where

import Test.Hspec
import Tsiolkovsky as SUT

spec :: Spec
spec = do
  context "Part 1" $ do
    it "12 => 2" $ do SUT.solve1 [12] `shouldBe` 2
    it "14 => 2" $ do SUT.solve1 [14] `shouldBe` 2
    it "1969 => 654" $ do SUT.solve1 [1969] `shouldBe` 654
    it "100756 => 33583" $ do SUT.solve1 [100756] `shouldBe` 33583
  context "Part 2" $ do
    it "14 => 2" $ do SUT.solve2 [14] `shouldBe` 2
    it "1969 => 966" $ do SUT.solve2 [1969] `shouldBe` 966
    it "100756 => 50346" $ do SUT.solve2 [100756] `shouldBe` 50346
