module Main where

import Test.Hspec
import Day8 as Day8

sampleInput = [ "b inc 5 if a > 1",
                "a inc 1 if b < 5",
                "c dec -10 if a >= 1",
                "c inc -20 if c == 10" ]

spec :: Spec
spec = do
  describe "Day 8" $ do
    context "part 1" $ do

      it "returns 1 for the sample input" $ do
        (fst $ Day8.solve sampleInput) `shouldBe` 1

    context "part 2 " $ do

      it "returns 10 for the sample input" $ do
        (snd $ Day8.solve sampleInput) `shouldBe` 10
main :: IO ()
main = hspec spec
