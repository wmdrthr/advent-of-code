module OrbitalMapSpec (spec) where

import Test.Hspec
import OrbitalMap as SUT

test_input_part1 = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
test_input_part2 = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

spec :: Spec
spec = do
  context "Part 1" $ do
    it "Sample Input -> 42" $ do
      SUT.solve1 (parse test_input_part1) `shouldBe` 42
  context "Part 2" $ do
    it "Sample Input -> 4" $ do
      SUT.solve2 (parse test_input_part2) `shouldBe` 4
