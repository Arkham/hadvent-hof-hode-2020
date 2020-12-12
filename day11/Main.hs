{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

{-
The seat layout fits neatly on a grid. Each position is either floor (.), an
empty seat (L), or an occupied seat (#). For example, the initial seat layout
might look like this:

L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL

All decisions are based on the number of occupied seats adjacent to a given
seat (one of the eight positions immediately up, down, left, right, or diagonal
from the seat). The following rules are applied to every seat simultaneously:

If a seat is empty (L) and there are no occupied seats adjacent to it, the seat
becomes occupied.  If a seat is occupied (#) and four or more seats adjacent to
it are also occupied, the seat becomes empty.  Otherwise, the seat's state does
not change.  Floor (.) never changes; seats don't move, and nobody sits on the
floor.

After one round of these rules, every seat in the example layout becomes occupied:

#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##

--- Part Two ---

As soon as people start to arrive, you realize your mistake. People don't just
care about adjacent seats - they care about the first seat they can see in each
of those eight directions!

Now, instead of considering just the eight immediately adjacent seats, consider
the first seat in each of those eight directions.
-}

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parseLine = P.parse rowParser ""
  let rows = rights $ map parseLine (T.lines contents)
  let layout = Matrix.fromLists rows

  let finalLayout = evolveUntilStabilized simpleEvolve layout
  print $ length $ filter (== Occupied) $ Matrix.toList finalLayout

  let otherLayout = evolveUntilStabilized fartherEvolve layout
  print $ length $ filter (== Occupied) $ Matrix.toList otherLayout

data Seat
  = Floor
  | Empty
  | Occupied
  deriving (Eq, Show)

type Layout = Matrix Seat

evolveUntilStabilized :: (Layout -> Layout) -> Layout -> Layout
evolveUntilStabilized evolveFn layout =
  if newLayout == layout
    then layout
    else evolveUntilStabilized evolveFn newLayout
  where
    newLayout = evolveFn layout

offsets :: [(Int, Int)]
offsets =
  [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

simpleEvolve :: Layout -> Layout
simpleEvolve layout =
  Matrix.mapPos
    ( \(r, c) elem ->
        let adjacentOccupied =
              countAdjacentOccupied (r, c)
         in if
                | elem == Empty && adjacentOccupied == 0 -> Occupied
                | elem == Occupied && adjacentOccupied >= 4 -> Empty
                | otherwise -> elem
    )
    layout
  where
    countAdjacentOccupied (r, c) =
      length $
        filter (== Occupied) $
          mapMaybe
            (\(i, j) -> Matrix.safeGet (r + i) (c + j) layout)
            offsets

fartherEvolve :: Layout -> Layout
fartherEvolve layout =
  Matrix.mapPos
    ( \(r, c) elem ->
        let visibleOccupied =
              countVisibleOccupied (r, c)
         in if
                | elem == Empty && visibleOccupied == 0 -> Occupied
                | elem == Occupied && visibleOccupied >= 5 -> Empty
                | otherwise -> elem
    )
    layout
  where
    countVisibleOccupied (r, c) =
      length $
        filter (== Occupied) $
          mapMaybe
            ( \(i, j) ->
                exploreDirection (r, c) (i, j)
            )
            offsets

    exploreDirection (r, c) (i, j) =
      let newR = r + i
          newC = c + j
       in case Matrix.safeGet newR newC layout of
            Just Floor ->
              exploreDirection (newR, newC) (i, j)
            other ->
              other

rowParser :: Parser [Seat]
rowParser =
  let floorParser = do
        P.char '.'
        return Floor

      emptyParser = do
        P.char 'L'
        return Empty

      occupiedParser = do
        P.char '#'
        return Occupied
   in P.many1 (P.choice [floorParser, emptyParser, occupiedParser])
