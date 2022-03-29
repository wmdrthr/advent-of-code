module DiveSpec (spec) where

import Test.Hspec
import Dive as SUT

spec :: Spec
spec = do
  let testInput = [(Forward, 5),
                   (Down, 5),
                   (Forward, 8),
                   (Up, 3),
                   (Down, 8),
                   (Forward, 2)]
  it "Part 1: without aim => 150" $ do SUT.solve1 testInput `shouldBe` 150
  it "Part 2: with aim => 900" $ do SUT.solve2 testInput `shouldBe` 900
