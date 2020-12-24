module Main where

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.ST as A
import Data.Char (digitToInt, intToDigit)
import Data.List ((\\))
import qualified Data.List as List

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
  print $ part2 (starting ++ ([1 .. 1000000] \\ starting))

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
part2 nums@(first : _) = runST $ do
  arr <- newArray' nums
  foldM_ (const . step arr) first $ replicate 10000000 ()
  y <- A.readArray arr 1
  z <- A.readArray arr y
  return $ y * z

newArray' :: [Int] -> ST s (A.STUArray s Int Int)
newArray' xs = do
  arr <- A.newArray_ (minimum xs, maximum xs)
  arr <$ zipWithM_ (A.writeArray arr) xs (drop 1 $ cycle xs)

-- we have something like
-- [x, a, b, c, y, ... , dest, next, ... ]
-- and we want to obtain
-- [x, y, ... , dest, a, b, c, next, ..., x ]
step :: A.STUArray s Int Int -> Int -> ST s Int
step arr x = do
  (low, high) <- A.getBounds arr
  a <- A.readArray arr x
  b <- A.readArray arr a
  c <- A.readArray arr b
  y <- A.readArray arr c
  let pred' num =
        if num == low
          then high
          else pred num
      dest = until (`notElem` [a, b, c]) pred' (pred' x)
  next <- A.readArray arr dest
  A.writeArray arr x y
  A.writeArray arr dest a
  A.writeArray arr c next
  pure y
