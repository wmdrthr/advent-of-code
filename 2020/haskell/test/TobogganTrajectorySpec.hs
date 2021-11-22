module TobogganTrajectorySpec (spec) where

import Test.Hspec
import TobogganTrajectory as SUT

testInput = [ "..##......."
            , "#...#...#.."
            , ".#....#..#."
            , "..#.#...#.#"
            , ".#...##..#."
            , "..#.##....."
            , ".#.#.#....#"
            , ".#........#"
            , "#.##...#..."
            , "#...##....#"
            , ".#..#...#.#"]

spec :: Spec
spec = do

  context "traverse" $ do
    it "test input with slope (3, 1) => 7 trees" $ do SUT.traverse testInput (3, 1) `shouldBe` 7
    it "test input with slope (1, 1) => 2 trees" $ do SUT.traverse testInput (1, 1) `shouldBe` 2
    it "test input with slope (5, 1) => 3 trees" $ do SUT.traverse testInput (5, 1) `shouldBe` 3
    it "test input with slope (7, 1) => 4 trees" $ do SUT.traverse testInput (7, 1) `shouldBe` 4
    it "test input with slope (1, 2) => 2 trees" $ do SUT.traverse testInput (1, 2) `shouldBe` 2
