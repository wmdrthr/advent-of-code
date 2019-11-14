module Main where

import Test.Hspec
import Day12 as Day12

testInput = ["0 <-> 2",
             "1 <-> 1",
             "2 <-> 0, 3, 4",
             "3 <-> 2, 4",
             "4 <-> 2, 3, 6",
             "5 <-> 6",
             "6 <-> 4, 5"]

spec :: Spec
spec = do
  describe "Day 12" $ do

    context "solve" $ do

      it "returns (6, 2) for the test input" $ do
        Day12.solve testInput `shouldBe` (6, 2)

main :: IO ()
main = hspec spec
