module Main where

import Test.Hspec
import Day14 as Day14


spec :: Spec
spec = do

  describe "Day 14" $ do

    context "solve1" $ do

      it "returns 8108 for 'flqrgnkx'" $ do
        Day14.solve1 "flqrgnkx" `shouldBe` 8108

      it "returns 8316 for 'ljoxqyyw'" $ do
        Day14.solve1 "ljoxqyyw" `shouldBe` 8316

    context "solve2" $ do

      it "returns 1242 for 'flqrgnkx'" $ do
        Day14.solve2 "flqrgnkx" `shouldBe` 1242

      it "returns 1074 for 'ljoxqyyw'" $ do
        Day14.solve2 "ljoxqyyw" `shouldBe` 1074

main :: IO ()
main = hspec spec
