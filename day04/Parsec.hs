{-# LANGUAGE OverloadedStrings #-}

module Parsec where

import Control.Monad (guard)
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Perm (permute, (<$$>), (<|?>), (<||>))
import Text.Parsec.Text (Parser)

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
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parsePassport = P.parse passportParser ""
  let passports = rights $ map parsePassport (T.splitOn "\n\n" contents)
  print $ length passports

data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: Height,
    hairColor :: T.Text,
    eyeColor :: T.Text,
    passportId :: T.Text,
    countryId :: Maybe Int
  }
  deriving (Eq, Show)

data Height
  = InCms Int
  | InInches Int
  deriving (Eq, Show)

passportParser :: Parser Passport
passportParser =
  permute $
    Passport <$$> eatSpaces byrParser
      <||> eatSpaces iyrParser
      <||> P.try (eatSpaces eyrParser)
      <||> P.try (eatSpaces heightParser)
      <||> P.try (eatSpaces hairColorParser)
      <||> P.try (eatSpaces eyeColorParser)
      <||> eatSpaces passportIdParser
      <|?> (Nothing, Just <$> eatSpaces countrIdParser)

eatSpaces :: Parser a -> Parser a
eatSpaces parser = do
  value <- parser
  P.spaces
  return value

yearParser :: String -> (Int, Int) -> Parser Int
yearParser value (rangeStart, rangeEnd) = do
  P.string value
  P.char ':'
  value <- P.count 4 P.digit
  let int = read value
  guard (int >= rangeStart && int <= rangeEnd)
  return int

byrParser :: Parser Int
byrParser = do
  yearParser "byr" (1920, 2002)

iyrParser :: Parser Int
iyrParser = do
  yearParser "iyr" (2010, 2020)

eyrParser :: Parser Int
eyrParser = do
  yearParser "eyr" (2020, 2030)

unitParser :: Int -> Parser Height
unitParser value =
  let cmParser = do
        P.string "cm"
        return (InCms value)

      inParser = do
        P.string "in"
        return (InInches value)
   in P.choice [cmParser, inParser]

heightParser :: Parser Height
heightParser = do
  P.string "hgt"
  P.char ':'
  digits <- P.many1 P.digit
  let value = read digits
  result <- unitParser value
  case result of
    InCms _ ->
      guard (value >= 150 && value <= 193)
    InInches _ ->
      guard (value >= 59 && value <= 76)
  return result

hairColorParser :: Parser T.Text
hairColorParser = do
  P.string "hcl"
  P.char ':'
  P.char '#'
  T.pack <$> P.count 6 (P.oneOf "0123456789abcdef")

eyeColorParser :: Parser T.Text
eyeColorParser = do
  P.string "ecl"
  P.char ':'
  T.pack
    <$> P.choice
      ( map
          (P.try . P.string)
          [ "amb",
            "blu",
            "brn",
            "gry",
            "grn",
            "hzl",
            "oth"
          ]
      )

passportIdParser :: Parser T.Text
passportIdParser = do
  P.string "pid"
  P.char ':'
  T.pack <$> P.count 9 P.digit

countrIdParser :: Parser Int
countrIdParser = do
  P.string "cid"
  P.char ':'
  value <- P.many1 P.digit
  return $ read value
