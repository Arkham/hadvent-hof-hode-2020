{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-
Specifically, they need you to find the two entries that sum to 2020 and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?
-}

{-
The Elves in accounting are thankful for your help; one of them even offers
you a starfish coin they had left over from a past vacation. They offer you a
second one if you can find three numbers in your expense report that meet the
same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366,
and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to
2020?
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"

  let numbers = map parseLine (T.lines contents)
  let numberSet = Set.fromList numbers
  let desired = 2020

  print $
    mapMaybe
      ( \current ->
          let other = desired - current
           in if Set.member other numberSet
                then Just $ current * other
                else Nothing
      )
      numbers

  print $
    mapMaybe
      ( \(first, second) ->
          let other = desired - first - second
           in if Set.member other numberSet
                then Just $ first * second * other
                else Nothing
      )
      (orderedTuples numbers [])

parseLine :: T.Text -> Integer
parseLine = read . T.unpack

orderedTuples :: [a] -> [(a, a)] -> [(a, a)]
orderedTuples [] acc = acc
orderedTuples (first : rest) acc =
  orderedTuples rest (acc <> map (first,) rest)

-- mainTwo :: IO ()
-- mainTwo = do
--   contents <- TI.readFile "input.txt"
--   let numbers = map parseLine (T.lines contents)
--   let numberSet = Set.fromList numbers
--   let desired = 2020
--   print $
--     mapMaybe
--       ( \first ->
--           let newSet =
--                 Set.delete first numberSet
--            in listToMaybe $
--                 mapMaybe
--                   ( \second ->
--                       let other = desired - first - second
--                        in if Set.member other newSet
--                             then Just $ first * second * other
--                             else Nothing
--                   )
--                   (Set.toList newSet)
--       )
--       numbers
