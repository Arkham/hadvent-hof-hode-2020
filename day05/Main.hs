{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-
Instead of zones or groups, this airline uses binary space partitioning to seat
people. A seat might be specified like FBFBBFFRLR, where F means "front", B
means "back", L means "left", and R means "right".

The first 7 characters will either be F or B; these specify exactly one of the
128 rows on the plane (numbered 0 through 127). Each letter tells you which
half of a region the given seat is in. Start with the whole list of rows; the
first letter indicates whether the seat is in the front (0 through 63) or the
back (64 through 127). The next letter indicates which half of that region the
seat is in, and so on until you're left with exactly one row.

For example, consider just the first seven characters of FBFBBFFRLR:

Start by considering the whole range, rows 0 through 127.
F means to take the lower half, keeping rows 0 through 63.
B means to take the upper half, keeping rows 32 through 63.
F means to take the lower half, keeping rows 32 through 47.
B means to take the upper half, keeping rows 40 through 47.
B keeps rows 44 through 47.
F keeps rows 44 through 45.
The final F keeps the lower of the two, row 44.

The last three characters will be either L or R; these specify exactly one of
the 8 columns of seats on the plane (numbered 0 through 7). The same process as
above proceeds again, this time with only three steps. L means to keep the
lower half, while R means to keep the upper half.

For example, consider just the last 3 characters of FBFBBFFRLR:

Start by considering the whole range, columns 0 through 7.
R means to take the upper half, keeping columns 4 through 7.
L means to take the lower half, keeping columns 4 through 5.
The final R keeps the upper of the two, column 5.

So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

Every seat also has a unique seat ID: multiply the row by 8, then add the
column. In this example, the seat has ID 44 * 8 + 5 = 357.

Here are some other boarding passes:

BFFFBBFRRR: row 70, column 7, seat ID 567.
FFFBBBFRRR: row 14, column 7, seat ID 119.
BBFFBBFRLL: row 102, column 4, seat ID 820.

As a sanity check, look through your list of boarding passes. What is the
highest seat ID on a boarding pass?

--- Part Two ---
Ding! The "fasten seat belt" signs have turned on. Time to find your seat.

It's a completely full flight, so your seat should be the only missing boarding
pass in your list. However, there's a catch: some of the seats at the very
front and back of the plane don't exist on this aircraft, so they'll be missing
from your list as well.

Your seat wasn't at the very front or back, though; the seats with IDs +1 and
-1 from yours will be in your list.

What is the ID of your seat?
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let seats = mapMaybe parseSeat (T.lines contents)
  let allSeatIds = seatId <$> seats
  let maxId = maximum allSeatIds
  let minId = minimum allSeatIds
  let setIds = Set.fromList allSeatIds
  print $ List.find (\id -> not (Set.member id setIds)) [minId .. maxId]

data Seat = Seat
  { row :: Int,
    column :: Int
  }
  deriving (Eq, Show)

seatId :: Seat -> Int
seatId Seat {row, column} = row * 8 + column

parseSeat :: T.Text -> Maybe Seat
parseSeat input =
  let (first, second) = T.splitAt 7 input
   in case (parseRow first, parseCol second) of
        (Just row, Just col) ->
          Just $ Seat row col
        _ ->
          Nothing

data BinaryOp
  = LowerHalf
  | UpperHalf

parseRow :: T.Text -> Maybe Int
parseRow input =
  let result =
        mapMaybe
          ( \case
              'F' -> Just LowerHalf
              'B' -> Just UpperHalf
              _ -> Nothing
          )
          (T.unpack input)
   in if length result == 7
        then Just $ parseBinaryOps (0, 127) result
        else Nothing

parseCol :: T.Text -> Maybe Int
parseCol input =
  let result =
        mapMaybe
          ( \case
              'L' -> Just LowerHalf
              'R' -> Just UpperHalf
              _ -> Nothing
          )
          (T.unpack input)
   in if length result == 3
        then Just $ parseBinaryOps (0, 7) result
        else Nothing

parseBinaryOps :: (Int, Int) -> [BinaryOp] -> Int
parseBinaryOps (rangeStart, _) [] = rangeStart
parseBinaryOps (rangeStart, rangeEnd) (first : rest) =
  let midPoint = rangeStart + div (rangeEnd - rangeStart) 2
   in case first of
        LowerHalf ->
          parseBinaryOps (rangeStart, midPoint) rest
        UpperHalf ->
          parseBinaryOps (midPoint + 1, rangeEnd) rest
