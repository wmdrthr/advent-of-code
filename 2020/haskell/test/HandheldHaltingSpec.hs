module HandheldHaltingSpec where

import Test.Hspec
import HandheldHalting as SUT

testInput = ["nop +0"
            ,"acc +1"
            ,"jmp +4"
            ,"acc +3"
            ,"jmp -3"
            ,"acc -99"
            ,"acc +1"
            ,"jmp -4"
            ,"acc +6"]

spec :: Spec
spec = do

  let program = map SUT.parse testInput
  context "Boot Code" $ do
    it "Part 1 => 5" $ do SUT.solve8a program `shouldBe` 5
    it "Part 1 => 8" $ do SUT.solve8b program `shouldBe` 8
