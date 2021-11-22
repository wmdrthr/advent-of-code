module SeatingSystemSpec where

import Test.Hspec
import           SeatingSystem as SUT
import qualified Data.Map      as M
import Data.List (intercalate)


nearestNeighborsTestInputs =
  [ ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#.....",
    ".............\n.L.L.#.#.#.#.\n.............",
    ".##.##.\n#.#.#.#\n##...##\n...L...\n##...##\n#.#.#.#\n.##.##."
  ]

testInput  = [ "L.LL.LL.LL",
               "LLLLLLL.LL",
               "L.L.L..L..",
               "LLLL.LL.LL",
               "L.LL.LL.LL",
               "L.LLLLL.LL",
               "..L.L.....",
               "LLLLLLLLLL",
               "L.LLLLLL.L",
               "L.LLLLL.LL"
             ]

spec :: Spec
spec = do

  context "neighbors" $ do
    let layout = Layout M.empty 10 10
    context "middle" $ do
      it "(4,4) -> [(4,3),(4,5),(3,4),(5,4),(3,3),(5,3),(3,5),(5,5)]" $ do
        SUT.neighbors layout (3,3) `shouldBe` [(3,2),(3,4),(2,3),(4,3),(2,2),(4,2),(2,4),(4,4)]
    context "corners" $ do
      it "(0,0) -> [(0,1),(1,0),(1,1)]" $ do SUT.neighbors layout (0,0) `shouldBe` [(0,1),(1,0),(1,1)]
      it "(9,0) -> [(9,1),(8,0),(8,1)]" $ do SUT.neighbors layout (9,0) `shouldBe` [(9,1),(8,0),(8,1)]
      it "(0,9) -> [(0,8),(1,9),(1,8)]" $ do SUT.neighbors layout (0,9) `shouldBe` [(0,8),(1,9),(1,8)]
      it "(9,9) -> [(9,8),(8,9),(8,8)]" $ do SUT.neighbors layout (9,9) `shouldBe` [(9,8),(8,9),(8,8)]
    context "edges" $ do
      it "(4,0) -> [(4,1),(3,0),(5,0),(3,1),(5,1)]" $ do
        SUT.neighbors layout (4,0) `shouldBe` [(4,1),(3,0),(5,0),(3,1),(5,1)]
      it "(0,4) -> [(0,3),(0,5),(1,4),(1,3),(1,5)]" $ do
        SUT.neighbors layout (0,4) `shouldBe` [(0,3),(0,5),(1,4),(1,3),(1,5)]
      it "(9,4) -> [(9,3),(9,5),(8,4),(8,3),(8,5)]" $ do
        SUT.neighbors layout (9,4) `shouldBe` [(9,3),(9,5),(8,4),(8,3),(8,5)]
      it "(4,9) -> [(4,8),(3,9),(5,9),(3,8),(5,8)]" $ do
        SUT.neighbors layout (9,4) `shouldBe` [(9,3),(9,5),(8,4),(8,3),(8,5)]

  context "nearestNeighbors" $ do
    it "Test Input 1 => 8 neighbors" $ do
      let layout = SUT.parse (nearestNeighborsTestInputs !! 0)
      SUT.nearestNeighbors layout (3, 4) `shouldBe` [(3,1),(3,8),(2,4),(8,4),(1,2),(7,0),(0,7),(4,5)]
    it "Test Input 2 => 1 neighbor" $ do
      let layout = SUT.parse (nearestNeighborsTestInputs !! 1)
      SUT.nearestNeighbors layout (1, 1) `shouldBe` [(3, 1)]
    it "Test Input 3 => 0 neighbors" $ do
      let layout = SUT.parse (nearestNeighborsTestInputs !! 2)
      SUT.nearestNeighbors layout (3, 3) `shouldBe` []

  context "testInput" $ do
    let layout = SUT.parse $ intercalate "\n" testInput
    it "Part 1 => 37" $ do SUT.solve11a layout `shouldBe` 37
    it "Part 2 => 26" $ do SUT.solve11b layout `shouldBe` 26
