module CalorieCountingSpec (spec) where

import Test.Hspec
import CalorieCounting (parseFood, solve1, solve2)

spec :: Spec
spec = do
    let testInput = [6000, 4000, 11000, 24000, 10000]

    it "Part 1 => 24000" $ do solve1 testInput `shouldBe` 24000
    it "Part 2 => 45000" $ do solve2 testInput `shouldBe` 45000

