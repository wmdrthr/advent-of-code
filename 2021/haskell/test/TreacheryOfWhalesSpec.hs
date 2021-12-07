module TreacheryOfWhalesSpec (spec) where

import Test.Hspec
import TreacheryOfWhales as SUT
import Data.List            (sort)

spec :: Spec
spec = do
  let testInput = sort [16,1,2,0,4,2,7,1,2,14]
  it "Part 1: simple    => 37" $ do SUT.solve1 testInput `shouldBe` 37
  it "Part 2: expensive => 168" $ do SUT.solve2 testInput `shouldBe` 168
