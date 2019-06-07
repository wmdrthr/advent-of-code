module Main where

import Test.Hspec
import Day7 as Day7

sampleInput = ["pbga (66)",
               "xhth (57)",
               "ebii (61)",
               "havc (66)",
               "ktlj (57)",
               "fwft (72) -> ktlj, cntj, xhth",
               "qoyq (66)",
               "padx (45) -> pbga, havc, qoyq",
               "tknk (41) -> ugml, padx, fwft",
               "jptl (61)",
               "ugml (68) -> gyxo, ebii, jptl",
               "gyxo (61)",
               "cntj (57)"]

spec :: Spec
spec = do
  describe "Day 7" $ do
    context "solve1" $ do

      it "returns 'tknk' for the sample input" $ do
        Day7.solve1 sampleInput `shouldBe` "tknk"

    context "solve2" $ do

      it "returns 60 for the sample input" $ do
        Day7.solve2 sampleInput `shouldBe` 60

main :: IO ()
main = hspec spec
