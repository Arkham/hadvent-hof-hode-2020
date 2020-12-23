{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (guard)
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Perm (permute, (<$$>), (<|?>), (<||>))
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "other-short-input.txt"
  let parsePassport = P.parse passportParser ""
  let passports = rights $ map parsePassport (T.splitOn "\n\n" contents)
  print $ length passports

passportParser :: Parser Passport
passportParser =
  permute $
    Passport <$$> byrParser
      <||> iyrParser
      <||> P.try eyrParser
      <||> P.try heightParser
      <||> P.try hairColorParser
      <||> P.try eyeColorParser
      <||> passportIdParser
      <|?> (Nothing, Just <$> countrIdParser)

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

yearParser :: String -> (Int, Int) -> Parser Int
yearParser value (rangeStart, rangeEnd) = do
  P.string value
  P.char ':'
  value <- P.count 4 P.digit
  P.spaces
  let int = read value
  guard (int >= rangeStart && int <= rangeEnd)
  return int

-- byr (Birth Year) - four digits; between 1920 and 2002.
byrParser :: Parser Int
byrParser = do
  yearParser "byr" (1920, 2002)

-- iyr (Issue Year) - four digits; between 2010 and 2020.
iyrParser :: Parser Int
iyrParser = do
  yearParser "iyr" (2010, 2020)

-- eyr (Expiration Year) - four digits; between 2020 and 2030.
eyrParser :: Parser Int
eyrParser = do
  yearParser "eyr" (2020, 2030)

-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be between 150 and 193.
-- If in, the number must be between 59 and 76.
heightParser :: Parser Height
heightParser = do
  P.string "hgt"
  P.char ':'
  digits <- P.many1 P.digit
  let value = read digits
  result <- unitParser value
  P.spaces
  case result of
    InCms _ ->
      guard (value >= 150 && value <= 193)
    InInches _ ->
      guard (value >= 59 && value <= 76)
  return result

unitParser :: Int -> Parser Height
unitParser value =
  let cmParser = do
        P.string "cm"
        return (InCms value)

      inParser = do
        P.string "in"
        return (InInches value)
   in P.choice [cmParser, inParser]

-- hcl (Hair Color) - a '#' followed by six chars 0-9 or a-f.
hairColorParser :: Parser T.Text
hairColorParser = do
  P.string "hcl"
  P.char ':'
  P.char '#'
  v <- P.count 6 (P.oneOf "0123456789abcdef")
  P.spaces
  return $ T.pack v

-- ecl (Eye Color) - one of: amb blu brn gry grn hzl oth.
eyeColorParser :: Parser T.Text
eyeColorParser = do
  P.string "ecl"
  P.char ':'
  v <-
    P.choice $
      map
        (P.try . P.string)
        [ "amb",
          "blu",
          "brn",
          "gry",
          "grn",
          "hzl",
          "oth"
        ]
  P.spaces
  return $ T.pack v

-- pid (Passport ID) - a nine-digit number.
passportIdParser :: Parser T.Text
passportIdParser = do
  P.string "pid"
  P.char ':'
  v <- P.count 9 P.digit
  P.spaces
  return $ T.pack v

-- cid (Country ID) - ignored, missing or not.
countrIdParser :: Parser Int
countrIdParser = do
  P.string "cid"
  P.char ':'
  value <- P.many1 P.digit
  P.spaces
  return $ read value
