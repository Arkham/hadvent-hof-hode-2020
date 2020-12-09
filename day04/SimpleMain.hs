{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isDigit, isSpace)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import GHC.Generics (Generic)

main :: IO ()
main = do
  contents <- TI.readFile "other-short-input.txt"
  let entries = map parseEntry (T.splitOn "\n\n" contents)
  print $ length $ filter isEntryValid entries

data Entry = Entry
  deriving (Show)

data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: Maybe Int
  }

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Eq, Show, Generic)

instance Hashable PassportField

type PassportEntry = HM.HashMap PassportField T.Text

parseEntry :: T.Text -> PassportEntry
parseEntry line =
  HM.fromList $
    mapMaybe parseField $
      T.split isSpace line

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

requiredFields :: [PassportField]
requiredFields =
  [ BirthYear,
    IssueYear,
    ExpirationYear,
    Height,
    HairColor,
    EyeColor,
    PassportId
  ]

isEntryValid :: PassportEntry -> Bool
isEntryValid entry =
  requiredFieldsPresent && allFieldsValid
  where
    requiredFieldsPresent =
      all
        (\field -> HM.member field entry)
        requiredFields
    allFieldsValid =
      all
        isFieldValid
        (HM.toList entry)

isFieldValid :: (PassportField, T.Text) -> Bool
isFieldValid (field, value) =
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
      case (T.length value, T.unpack value) of
        (7, '#' : rest) ->
          all (`elem` allowedHexChars) rest
        _ ->
          False
    EyeColor ->
      value
        `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    PassportId ->
      T.length value == 9 && T.all isDigit value
    CountryId ->
      T.all isDigit value
  where
    toInt :: T.Text -> Int
    toInt = read . T.unpack

    allowedHexChars :: [Char]
    allowedHexChars = ['0' .. '9'] <> ['a' .. 'f']
