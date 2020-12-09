{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let numbers = map toInt (T.lines contents)
  case findFirstNonMatching 25 numbers of
    Just invalidNumber -> do
      print invalidNumber
      case findSequenceSummingTo invalidNumber numbers of
        Just seq ->
          print (maximum seq + minimum seq)
        Nothing ->
          print "No summing sequence found"
    Nothing ->
      print "No invalid number found"

findFirstNonMatching :: Int -> [Int] -> Maybe Int
findFirstNonMatching preambleLength numbers =
  let (preamble, rest) = List.splitAt preambleLength numbers
   in checkAndUpdatePreamble preamble rest (Set.fromList preamble)

checkAndUpdatePreamble :: [Int] -> [Int] -> Set.Set Int -> Maybe Int
checkAndUpdatePreamble [] _ _ = Nothing
checkAndUpdatePreamble _ [] _ = Nothing
checkAndUpdatePreamble (firstPreamble : restPreamble) (first : rest) preambleSet =
  let isFirstSumOfPreable =
        any
          (\elem -> Set.member (first - elem) preambleSet)
          preambleSet

      updatedPreambleSet =
        Set.insert first $
          Set.delete firstPreamble preambleSet
   in if isFirstSumOfPreable
        then checkAndUpdatePreamble (restPreamble ++ [first]) rest updatedPreambleSet
        else Just first

findSequenceSummingTo :: Int -> [Int] -> Maybe [Int]
findSequenceSummingTo _ [] = Nothing
findSequenceSummingTo desired numbers@(_ : rest) =
  case sumFoundFor desired numbers of
    Just found ->
      Just found
    Nothing ->
      findSequenceSummingTo desired rest

sumFoundFor :: Int -> [Int] -> Maybe [Int]
sumFoundFor desired [] = Nothing
sumFoundFor desired numbers =
  let numbersWithSum =
        scanl
          ( \(acc, lastSum) elem ->
              (elem : acc, elem + lastSum)
          )
          ([], 0)
          numbers

      whileSumLessThanDesired =
        takeWhile
          (\(num, sum) -> sum <= desired)
          numbersWithSum

      (lastAcc, lastSum) =
        List.last whileSumLessThanDesired
   in if lastSum == desired
        then Just lastAcc
        else Nothing

toInt :: T.Text -> Int
toInt = read . T.unpack
