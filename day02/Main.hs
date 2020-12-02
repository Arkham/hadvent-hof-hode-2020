{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits (xor)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-
To try to debug the problem, they have created a list (your puzzle input) of
passwords (according to the corrupted database) and the corporate policy when
that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy
indicates the lowest and highest number of times a given letter must appear for
the password to be valid. For example, 1-3 a means that the password must
contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is
not; it contains no instances of b, but needs at least 1. The first and third
passwords are valid: they contain one a or nine c, both within the limits of
their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---
While it appears you validated the passwords correctly, they don't seem to
be what the Official Toboggan Corporate Authentication System is
expecting.

The shopkeeper suddenly realizes that he just accidentally explained the
password policy rules from his old job at the sled rental place down the
street! The Official Toboggan Corporate Policy actually works a little
differently.

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so on. (Be
careful; Toboggan Corporate Policies have no concept of "index zero"!)
Exactly one of these positions must contain the given letter. Other
occurrences of the letter are irrelevant for the purposes of policy
enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

How many passwords are valid according to the new interpretation of the
policies?
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let lines = mapMaybe parseLine (T.lines contents)
  print $ length $ filter isPasswordValid lines
  print $ length $ filter isPasswordValidTwo lines

data PasswordRow = PasswordRow
  { req :: (Int, Int, Char),
    password :: T.Text
  }

parseLine :: T.Text -> Maybe PasswordRow
parseLine line =
  case T.splitOn " " line of
    [range, needle, password] ->
      case T.splitOn "-" range of
        [start, end] ->
          Just $
            PasswordRow
              { req =
                  ( read $ T.unpack start,
                    read $ T.unpack end,
                    T.head needle
                  ),
                password = password
              }
        _ ->
          Nothing
    _ ->
      Nothing

isPasswordValid :: PasswordRow -> Bool
isPasswordValid PasswordRow {req, password} =
  let (min, max, needle) = req
      count = length $ filter (needle ==) $ T.unpack password
   in count >= min && count <= max

isPasswordValidTwo :: PasswordRow -> Bool
isPasswordValidTwo PasswordRow {req, password} =
  let (first, second, needle) = req
      foundAt index = T.index password (index - 1) == needle
   in xor (foundAt first) (foundAt second)
