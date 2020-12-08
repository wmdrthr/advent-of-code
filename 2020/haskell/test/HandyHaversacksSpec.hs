module HandyHaversacksSpec where

import           Test.Hspec
import           HandyHaversacks as SUT
import qualified Data.Map        as M

testCases =
  [ ["light red bags contain 1 bright white bag, 2 muted yellow bags."
     ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
     ,"bright white bags contain 1 shiny gold bag."
     ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
     ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
     ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
     ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
     ,"faded blue bags contain no other bags."
     ,"dotted black bags contain no other bags."],
    ["shiny gold bags contain 2 dark red bags."
     ,"dark red bags contain 2 dark orange bags."
     ,"dark orange bags contain 2 dark yellow bags."
     ,"dark yellow bags contain 2 dark green bags."
     ,"dark green bags contain 2 dark blue bags."
     ,"dark blue bags contain 2 dark violet bags."
     ,"dark violet bags contain no other bags."]
  ]

spec :: Spec
spec = do

  context "Test Case 1" $ do
    let rules = foldl SUT.parseRules M.empty (testCases !! 0)
    it "Part 1 => 4" $ do SUT.solve7a rules `shouldBe` 4
    it "Part 2 => 32" $ do SUT.solve7b rules `shouldBe` 32
  context "Test Case 2" $ do
    let rules = foldl SUT.parseRules M.empty (testCases !! 1)
    it "Part 1 => 0" $ do SUT.solve7a rules `shouldBe` 0
    it "Part 2 => 126" $ do SUT.solve7b rules `shouldBe` 126
