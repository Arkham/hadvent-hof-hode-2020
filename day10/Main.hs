{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let numbers = List.sort $ map toInt (T.lines contents)
  print numbers
  let diffs = findJoltDifferences numbers
  print diffs
  let ones = length $ filter (== 1) diffs
  let threes = length $ filter (== 3) diffs
  print (ones, threes, ones * threes)
  print $ product $ map countCompositions $ Split.splitWhen (== 3) diffs

toInt :: T.Text -> Int
toInt = read . T.unpack

findJoltDifferences :: [Int] -> [Int]
findJoltDifferences numbers =
  let result =
        snd $
          foldl
            ( \(lastNum, acc) num ->
                (num, (num - lastNum) : acc)
            )
            -- charging outlet is 0 jolts
            (0, [])
            numbers
   in -- our devices'built-in adapter in 3 higher than highest adapter
      reverse (3 : result)

-- there are no diffs with jolt 2 in the data, so we can worry only about ones
-- we want to count how many ways exist to transform sequences in this fashion
--
-- 0) [1]       -> [[1]]
-- 1) [1,1]     -> [[1,1], [2]]
-- 2) [1,1,1]   -> [[1,1,1], [1,2], [2,1], [3]]
-- 3) [1,1,1,1] -> [[1,1,1,1], [1,1,2], [1,2,1], [1,3], [2,1,1], [2,2], [3,1]]
--
-- if you look at f(3) you can notice that:
-- if the sequence ends in 1, it's the same as f(2) with 1 added to each
-- if the sequence ends in 2, it's the same as f(1) with 2 added to each
-- if the sequence ends in 3, it's the same as f(0) with 3 added to each
--
-- so the generalization is count(n) = count(n-1) + count(n-2) + count(n-3)
countCompositions :: [Int] -> Int
countCompositions = tribonacci . length

tribonacci :: Int -> Int
tribonacci 0 = 1
tribonacci 1 = 1
tribonacci 2 = 2
tribonacci n = go 1 2 4 (n - 2)
  where
    go a b c 1 = c
    go a b c n = go b c (a + b + c) (n - 1)
