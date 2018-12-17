module AlchemicalReductionSpec (spec) where

import Test.Hspec
import AlchemicalReduction

spec :: Spec
spec = do
  context "part 1" $ do
    it "dabAcCaCBAcCcaDA => 10" $ do
      AlchemicalReduction.solve1 "dabAcCaCBAcCcaDA" `shouldBe` 10
  context "part 2" $ do
    it "dabAcCaCBAcCcaDA => 4" $ do
      AlchemicalReduction.solve2 "dabAcCaCBAcCcaDA" `shouldBe` 4
