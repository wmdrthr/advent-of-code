module AdventOfCodeSpec (spec) where

import Test.Hspec
import AdventOfCode as AoC

import qualified Data.Map as M

spec :: Spec
spec = do

  context "parseInputSimple" $ do
    it "0 -> []" $ AoC.parseInputSimple "0" `shouldBe` []
    it "44545 -> [4, 4, 5, 4, 5]" $ AoC.parseInputSimple "44545" `shouldBe` [4, 4, 5, 4, 5]

  context "parseInputNumbers" $ do
    it "43\\n3\\n\\n10 -> [43, 3, 4, 10]" $ AoC.parseInputNumbers "43\n3\n4\n10" `shouldBe` [43, 3, 4, 10]

  context "parseInputNumbersWithSign" $ do
    it "-10\\n+14\\n-15\\n+6\\n+2\\n+6 -> [-10, 14, -15, 6, 2, 6]" $
      AoC.parseInputNumbersWithSign "-10\n+14\n-15\n+6\n+2\n+6" `shouldBe` [-10, 14, -15, 6, 2, 6]

  context "parseCommaSeparatedNumbers" $ do
    it "79,1,10,79,83,2" $ AoC.parseCommaSeparatedNumbers "79,1,10,79,83,2" `shouldBe` [79, 1, 10, 79, 83, 2]

  context "cardinality" $ do
    it "cardinality \"foobar\" -> [('a',1),('b',1),('f',1),('o',2),('r',1)]" $
      AoC.cardinality "foobar" `shouldBe` M.fromList [('a',1),('b',1),('f',1),('o',2),('r',1)]
    it "cardinality [1,2,3,1,2,3,1,2,3,4,2] -> [(1,3),(2,4),(3,3),(4,1)]" $
      AoC.cardinality [1,2,3,1,2,3,1,2,3,4,2] `shouldBe` M.fromList [(1,3),(2,4),(3,3),(4,1)]

  context "parseInputSimple" $ do
    it "1212 -> [1,2,1,2]" $ do AoC.parseInputSimple "1212" `shouldBe` [1,2,1,2]

  context "combinations" $ do
    it "[0..2] C 2 -> [[0,1], [0,2], [1,2]]" $ do AoC.combinations 2 [0..2] `shouldBe` [[0,1], [0,2], [1,2]]
    it "\"abcde\" C 3 -> ... " $
      do AoC.combinations 3 ['A', 'B', 'C', 'D', 'E'] `shouldBe` [['A', 'B', 'C'],
                                                                  ['A', 'B', 'D'],
                                                                  ['A', 'B', 'E'],
                                                                  ['A', 'C', 'D'],
                                                                  ['A', 'C', 'E'],
                                                                  ['A', 'D', 'E'],
                                                                  ['B', 'C', 'D'],
                                                                  ['B', 'C', 'E'],
                                                                  ['B', 'D', 'E'],
                                                                  ['C', 'D', 'E']]

