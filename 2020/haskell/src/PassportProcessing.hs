module PassportProcessing where


import Data.Maybe      (fromJust)
import Data.List       (isSuffixOf)
import Data.List.Split (splitOn)
import Text.Regex.PCRE

type Passport = [(String, String)]

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport = all (\x -> elem x keys) ["byr","ecl","eyr","hcl","hgt","iyr","pid"]
  where keys = map fst passport

validYear :: String -> (Int, Int) -> Passport -> Bool
validYear key (min, max) passport = birthYear >= min && birthYear <= max
  where birthYear = read $ fromJust (lookup key passport)

validBirthYear      = validYear "byr" (1920, 2002)
validIssuingYear    = validYear "iyr" (2010, 2020)
validExpirationYear = validYear "eyr" (2020, 2030)

validHeight :: Passport -> Bool
validHeight passport = validMeasurement $ fromJust (lookup "hgt" passport)
  where validMeasurement h
          | "cm" `isSuffixOf` h = parsedHeight >= 150 && parsedHeight <= 193
          | "in" `isSuffixOf` h = parsedHeight >= 59  && parsedHeight <= 76
          | otherwise           = False
          where parsedHeight = read (take ((length h) - 2) h)

validEyeColor :: Passport -> Bool
validEyeColor passport = elem (fromJust (lookup "ecl" passport)) ["amb", "blu", "brn", "gry",
                                                                  "grn", "hzl", "oth"]

validHairColor :: Passport -> Bool
validHairColor passport = (fromJust (lookup "hcl" passport)) =~ "^#[0-9a-f]{6}$"

validPassportID :: Passport -> Bool
validPassportID passport = (fromJust (lookup "pid" passport)) =~ "^\\d{9}$"

validPassport :: Passport -> Bool
validPassport passport = hasRequiredFields passport &&
                         validBirthYear passport &&
                         validIssuingYear passport &&
                         validExpirationYear passport &&
                         validHeight passport &&
                         validEyeColor passport &&
                         validHairColor passport &&
                         validPassportID passport

parsePassport :: String -> Passport
parsePassport = map (\l -> (head l, l !! 1)) . map (splitOn ":") . words

day04 :: String -> IO ()
day04 input = do
  let passports = map parsePassport $ splitOn "\n\n" input
  print $ length $ filter hasRequiredFields passports
  print $ length $ filter validPassport passports
