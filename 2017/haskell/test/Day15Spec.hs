module Main where

import Test.Hspec
import Day15 as Day15

spec :: Spec
spec = do

  describe "Day 15" $ do

    context "solve1" $ do

      it "returns 588 for (65, 8921)" $ do
        Day15.solve1 (65, 8921) `shouldBe` 588

      it "returns 609 for (883, 879)" $ do
        Day15.solve1 (883, 879) `shouldBe` 609

    context "solve2" $ do

      it "returns 309 for (65, 8921)" $ do
        Day15.solve2 (65, 8921) `shouldBe` 309

      it "returns 253 for (883, 879)" $ do
        Day15.solve2 (883, 879) `shouldBe` 253


main :: IO ()
main = hspec spec
