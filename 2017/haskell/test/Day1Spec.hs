module Main where

import Test.Hspec
import Day1 as Day1

spec :: Spec
spec = do
  describe "Day 1" $ do
    context "solve1" $ do

      it "returns 3 for 1122" $ do
        Day1.solve1 [1, 1, 2, 2] `shouldBe` 3

      it "returns 4 for 1111" $ do
        Day1.solve1 [1, 1, 1, 1] `shouldBe` 4

      it "returns 0 for 1234" $ do
        Day1.solve1 [1, 2, 3, 4] `shouldBe` 0

      it "returns 9 for 91212129" $ do
        Day1.solve1 [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9

    context "solve2" $ do

      it "returns 6 for 1212" $ do
        Day1.solve2 [1, 2, 1, 2] `shouldBe` 6

      it "returns 0 for 1221" $ do
        Day1.solve2 [1, 2, 2, 1] `shouldBe` 0

      it "returns 4 for 123425" $ do
        Day1.solve2 [1, 2, 3, 4, 2, 5] `shouldBe` 4

      it "returns 12 for 123123" $ do
        Day1.solve2 [1, 2, 3, 1, 2, 3] `shouldBe` 12

      it "returns 4 for 12131415" $ do
        Day1.solve2 [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4

main :: IO ()
main = hspec spec
