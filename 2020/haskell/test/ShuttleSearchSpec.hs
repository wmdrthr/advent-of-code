module ShuttleSearchSpec where

import Test.Hspec
import ShuttleSearch as SUT

spec :: Spec
spec = do

  context "939, 7,13,x,x,59,x,31,19" $ do
    let input = ["939", "7,13,x,x,59,x,31,19"]
    let delay = read (head input) :: Integer
    let buses = parse (last input)

    it "Part 1 => 295" $ do SUT.solve13a delay buses `shouldBe` 295
    it "Part 2 => 1068781" $ do SUT.solve13b buses `shouldBe` 1068781

  context "Part 2 Extra Checks" $ do
    it "17,x,13,19 => 3417" $ do SUT.solve13b (parse "17,x,13,19") `shouldBe` 3417
    it "67,7,59,61 => 754018" $ do SUT.solve13b (parse "67,7,59,61") `shouldBe` 754018
    it "67,x,7,59,61 => 779210" $ do SUT.solve13b (parse "67,x,7,59,61") `shouldBe` 779210
    it "67,7,x,59,61 => 1261476" $ do SUT.solve13b (parse "67,7,x,59,61") `shouldBe` 1261476
    it "1789,37,47,1889 => 1202161486" $ do SUT.solve13b (parse "1789,37,47,1889") `shouldBe` 1202161486
