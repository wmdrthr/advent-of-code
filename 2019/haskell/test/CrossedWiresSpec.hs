module CrossedWiresSpec (spec) where

import Test.Hspec
import CrossedWires as SUT

spec :: Spec
spec = do
  context "Part 1" $ do
    it "Sample Input 1 -> 6" $ do
      SUT.solve1 ["R8,U5,L5,D3","U7,R6,D4,L4"] `shouldBe` 6
    it "Sample Input 2 -> 159" $ do
      SUT.solve1 ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                  "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 159
    it "Sample Input 3 -> 135" $ do
      SUT.solve1 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] `shouldBe` 135
  context "Part 2" $ do
    it "Sample Input 1 -> 30" $ do
      SUT.solve2 ["R8,U5,L5,D3","U7,R6,D4,L4"] `shouldBe` 30
    it "Sample Input 2 -> 610" $ do
      SUT.solve2 ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                  "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 610
    it "Sample Input 3 -> 410" $ do
      SUT.solve2 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] `shouldBe` 410
