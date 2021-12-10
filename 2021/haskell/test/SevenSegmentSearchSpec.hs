module SevenSegmentSearchSpec (spec) where

import Test.Hspec
import SevenSegmentSearch as SUT

spec :: Spec
spec = do
  let testInput = [["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb", "fdgacbe cefdb cefbgd gcbe"],
                   ["edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec", "fcgedb cgb dgebacf gc"],
                   ["fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef", "cg cg fdcagb cbg"],
                   ["fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega", "efabcd cedba gadfec cb"],
                   ["aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga", "gecf egdcabf bgf bfgea"],
                   ["fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf", "gebdcfa ecba ca fadegcb"],
                   ["dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf", "cefg dcbef fcge gbcadfe"],
                   ["bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd", "ed bcgafe cdgba cbgef"],
                   ["egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg", "gbdfcae bgc cg cgb"],
                   ["gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc", "fgae cfgab fg bagce"]]

  it "Part 1: simple => 26" $ do SUT.solve1 (map (!!1) testInput) `shouldBe` 26
  it "Part 2: decoded => 61229" $ do SUT.solve2 testInput `shouldBe` 61229
