module DockingDataSpec where

import Test.Hspec
import DockingData as SUT

testInputs =
  [ ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"],
    ["mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"] ]

spec :: Spec
spec = do

  context "Part 1" $ do
    it "Test Case 1 > 165" $ do SUT.solve14a (map parse (testInputs !! 0)) `shouldBe` 165
    it "Test Case 2 > 51" $ do SUT.solve14a (map parse (testInputs !! 1)) `shouldBe` 51

  context "Part 2" $ do
    it "Test Case 2 > 208" $ do SUT.solve14b (map parse (testInputs !! 1)) `shouldBe` 208
