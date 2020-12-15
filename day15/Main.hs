{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

main :: IO ()
main = do
  let numbers = [7, 14, 0, 17, 11, 1, 2]
  let initial = HM.fromList (zip numbers (map (\n -> (n, n)) [1 ..]))
  print $ findNthNumberSpoken 30000000 (List.length numbers, List.last numbers) initial

findNthNumberSpoken :: Int -> (Int, Int) -> HM.HashMap Int (Int, Int) -> Int
findNthNumberSpoken desired (lastIndex, lastNumber) memory
  | desired == lastIndex = lastNumber
  | otherwise =
    case HM.lookup lastNumber memory of
      Just (lastTurn, beforeThat) ->
        readAndUpdateMemory (lastTurn - beforeThat)
      _ ->
        readAndUpdateMemory 0
  where
    currentIndex = lastIndex + 1

    readAndUpdateMemory num =
      findNthNumberSpoken
        desired
        (currentIndex, num)
        ( HM.alter
            ( \case
                Just (lastIndex, _) ->
                  Just (currentIndex, lastIndex)
                _ ->
                  Just (currentIndex, currentIndex)
            )
            num
            memory
        )
