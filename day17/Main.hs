{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse gridParser "" contents of
    Right initialGrid -> do
      print initialGrid
      print $ length $ evolve 6 initialGrid
    Left err ->
      print err

type Grid = Set.Set (Int, Int, Int, Int)

data Token
  = Active
  | Inactive
  deriving (Eq, Show)

evolve :: Int -> Grid -> Grid
evolve 0 grid = grid
evolve n grid =
  evolve (n -1) $
    Set.fromList $
      mapMaybe
        ( \coords ->
            let activeCount = activeNeighboursCount coords grid
             in case (Set.member coords grid, activeCount) of
                  (True, 2) ->
                    Just coords
                  (True, 3) ->
                    Just coords
                  (False, 3) ->
                    Just coords
                  _ ->
                    Nothing
        )
        points
  where
    range (min, max) =
      [(min - 1) .. (max + 1)]

    (forX, forY, forZ, forW) = findMinMaxes grid

    points =
      [(x, y, z, w) | x <- range forX, y <- range forY, z <- range forZ, w <- range forW]

offsets :: [(Int, Int, Int, Int)]
offsets =
  [ (x, y, z, w) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], w <- [-1 .. 1], (x, y, z, w) /= (0, 0, 0, 0)
  ]

activeNeighboursCount :: (Int, Int, Int, Int) -> Grid -> Int
activeNeighboursCount (x, y, z, w) grid =
  length $
    filter
      ( \(xOff, yOff, zOff, wOff) ->
          Set.member (x + xOff, y + yOff, z + zOff, w + wOff) grid
      )
      offsets

findMinMaxes :: Grid -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
findMinMaxes grid =
  let values = Set.toList grid
      xs = map (\(x, _, _, _) -> x) values
      ys = map (\(_, y, _, _) -> y) values
      zs = map (\(_, _, z, _) -> z) values
      ws = map (\(_, _, _, w) -> w) values
   in ((minimum xs, maximum xs), (minimum ys, maximum ys), (minimum zs, maximum zs), (minimum ws, maximum ws))

gridParser :: Parser Grid
gridParser = do
  rows <- P.endBy rowParser P.newline
  return $
    Set.fromList $
      concat $
        zipWith
          ( \row rowIndex ->
              List.map (,rowIndex,0,0) $
                mapMaybe
                  ( \(el, colIndex) ->
                      case el of
                        Active ->
                          Just colIndex
                        Inactive ->
                          Nothing
                  )
                  (zip row [0 ..])
          )
          rows
          [0 ..]
  where
    rowParser :: Parser [Token]
    rowParser = do
      P.many1
        ( P.choice
            [ Active <$ P.char '#',
              Inactive <$ P.char '.'
            ]
        )
