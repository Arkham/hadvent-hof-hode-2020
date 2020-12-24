{-# LANGUAGE Strict #-}

module Main where

import Data.Char (digitToInt, intToDigit)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as M

main :: IO ()
main = do
  let input = "792845136"
  -- let input = "389125467"
  let starting = map digitToInt input
  let final =
        foldl
          (\acc _idx -> performMove 9 acc)
          starting
          [1 .. 100]
  print $ part1 final
  print $ part2 (starting ++ [10 .. 1000000])

performMove :: Int -> [Int] -> [Int]
performMove max (x : y1 : y2 : y3 : ys) =
  let findDestination rest current =
        case (List.elemIndex current rest, current > 0) of
          (Just index, _) ->
            let (firstHalf, secondHalf) = List.splitAt (index + 1) rest
             in firstHalf <> [y1, y2, y3] <> secondHalf <> [x]
          (Nothing, True) ->
            findDestination rest (current - 1)
          (Nothing, False) ->
            findDestination rest max
   in findDestination ys (x - 1)

part1 :: [Int] -> Either String String
part1 nums =
  case List.elemIndex 1 nums of
    Nothing ->
      Left "1 not found in final sequence"
    Just idx ->
      let (firstHalf, secondHalf) = List.splitAt idx nums
       in Right $ map intToDigit $ drop 1 secondHalf <> firstHalf

part2 :: [Int] -> Int
part2 nums@(first : rest) =
  let storage = M.fromList $ zip nums (rest ++ [first])
      result = snd (repeatLots 10000000 (first, storage))
      p1 = result ! 1
      p2 = result ! p1
   in p1 * p2

repeatLots :: Int -> (Int, M.Map Int Int) -> (Int, M.Map Int Int)
repeatLots 0 acc = acc
repeatLots n acc = repeatLots (n - 1) (step acc)

-- we have something like
-- (x) -> a -> b -> c -> y -> ... -> dest -> next -> ... -> x
-- and we want to obtain
-- x -> (y) -> ... -> dest -> a -> b -> c -> next -> ... -> x
step :: (Int, M.Map Int Int) -> (Int, M.Map Int Int)
step (x, storage) =
  let max = M.size storage
      a = storage ! x
      b = storage ! a
      c = storage ! b
      y = storage ! c
      pred' num =
        if num == 1
          then max
          else pred num
      dest = until (`notElem` [a, b, c]) pred' (pred' x)
      next = storage ! dest
      newStorage =
        M.insert x y $
          M.insert dest a $
            M.insert c next storage
   in (y, newStorage)
