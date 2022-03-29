module HydrothermalVentureSpec (spec) where

import Test.Hspec
import HydrothermalVenture as SUT


spec :: Spec
spec = do
  let testInput = [((0, 9), (5, 9)),
                    ((8, 0), (0, 8)),
                    ((9, 4), (3, 4)),
                    ((2, 2), (2, 1)),
                    ((7, 0), (7, 4)),
                    ((6, 4), (2, 0)),
                    ((0, 9), (2, 9)),
                    ((3, 4), (1, 4)),
                    ((0, 0), (8, 8)),
                    ((5, 5), (8, 2))]
  it "Part 1: no diagonals   => 5" $ do SUT.solve1 testInput `shouldBe` 5
  it "Part 2: with diagonals => 12" $ do SUT.solve testInput `shouldBe` 12
