module AdventOfCodeSpec (spec) where

import Test.Hspec
import AdventOfCode as AoC

spec :: Spec
spec = do

  context "parseInputSimple" $ do
    it "1212 -> [1,2,1,2]" $ do AoC.parseInputSimple "1212" `shouldBe` [1,2,1,2]
