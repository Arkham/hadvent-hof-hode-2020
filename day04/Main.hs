{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import GHC.Generics (Generic)

{-
The automatic passport scanners are slow because they're having trouble
detecting which passports have all required fields. The expected fields are as
follows:

byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is
represented as a sequence of key:value pairs separated by spaces or newlines.

Passports are separated by blank lines.

Here is an example batch file containing four passports:

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present.

The second passport is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks
like data from North Pole Credentials, not a passport at all! Surely, nobody
would mind if you made the system temporarily ignore missing cid fields. Treat
this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine,
but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

Count the number of valid passports - those that have all required fields.
Treat cid as optional. In your batch file, how many passports are valid?

--- Part Two ---

The line is moving more quickly now, but you overhear airport security talking
about how passports with invalid data are getting through. Better add some data
validation, quick!

You can continue to ignore the cid field, but each other field has strict rules
about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both present
and valid according to the above rules.
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let passportEntries = map parseEntry (T.splitOn "\n\n" contents)
  print $ length $ mapMaybe entryToPassport passportEntries

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Eq, Generic)

instance Hashable PassportField

type PassportEntry = HM.HashMap PassportField T.Text

parseEntry :: T.Text -> PassportEntry
parseEntry line =
  HM.fromList $
    mapMaybe parseField $
      T.split (\c -> c == ' ' || c == '\n') line

parseField :: T.Text -> Maybe (PassportField, T.Text)
parseField value =
  case T.splitOn ":" value of
    ["byr", byr] ->
      Just (BirthYear, byr)
    ["iyr", iyr] ->
      Just (IssueYear, iyr)
    ["eyr", eyr] ->
      Just (ExpirationYear, eyr)
    ["hgt", height] ->
      Just (Height, height)
    ["hcl", color] ->
      Just (HairColor, color)
    ["ecl", color] ->
      Just (EyeColor, color)
    ["pid", pid] ->
      Just (PassportId, pid)
    ["cid", cid] ->
      Just (CountryId, cid)
    _ ->
      Nothing

data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: T.Text,
    hairColor :: T.Text,
    eyeColor :: T.Text,
    passportId :: T.Text,
    countryId :: Maybe Int
  }

fieldValidation :: (PassportField, T.Text) -> Maybe (PassportField, T.Text)
fieldValidation (field, value) =
  if isValid
    then Just (field, value)
    else Nothing
  where
    isValid =
      case field of
        BirthYear ->
          let v = toInt value
           in T.length value == 4 && v >= 1920 && v <= 2002
        IssueYear ->
          let v = toInt value
           in T.length value == 4 && v >= 2010 && v <= 2020
        ExpirationYear ->
          let v = toInt value
           in T.length value == 4 && v >= 2020 && v <= 2030
        Height ->
          case T.span isDigit value of
            (num, "cm") ->
              let n = toInt num
               in n >= 150 && n <= 193
            (num, "in") ->
              let n = toInt num
               in n >= 59 && n <= 76
            _ ->
              False
        HairColor ->
          case T.unpack value of
            '#' : rest ->
              all (\c -> c `elem` (['0' .. '9'] <> ['a' .. 'f'])) rest
            _ ->
              False
        EyeColor ->
          value
            `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        PassportId ->
          T.length value == 9 && T.all isDigit value
        CountryId ->
          True

entryToPassport :: PassportEntry -> Maybe Passport
entryToPassport entry =
  let requiredFields =
        [BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportId]

      getAllRequiredFields e =
        traverse
          ( \field ->
              HM.lookup field e
                >>= \v ->
                  snd <$> fieldValidation (field, v)
          )
          requiredFields
   in case getAllRequiredFields entry of
        Just [byr, iyr, eyr, hgt, hcl, ecl, pid] ->
          Just $
            Passport
              { birthYear = toInt byr,
                issueYear = toInt iyr,
                expirationYear = toInt eyr,
                height = hgt,
                hairColor = hcl,
                eyeColor = ecl,
                passportId = pid,
                countryId = toInt <$> HM.lookup CountryId entry
              }
        _ ->
          Nothing

toInt :: T.Text -> Int
toInt = read . T.unpack
