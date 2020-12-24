{-# LANGUAGE OverloadedStrings #-}

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
  case P.parse inputParser "" contents of
    Left err ->
      print err
    Right input -> do
      let initialBlackTiles =
            Set.fromList $
              concat $
                filter (odd . length) $
                  List.group $
                    List.sort $
                      map movesPosition input
      print $ length initialBlackTiles
      print $ map length (take 101 $ iterate evolve initialBlackTiles)

data Move
  = East
  | SouthEast
  | SouthWest
  | West
  | NorthEast
  | NorthWest
  deriving (Eq, Show, Enum, Bounded)

allMoves :: [Move]
allMoves =
  [minBound .. maxBound]

evolve :: Set.Set (Float, Float) -> Set.Set (Float, Float)
evolve blackTiles =
  Set.fromList $
    concatMap
      ( \current ->
          let neighbours (x, y) =
                map
                  ( \move ->
                      let (offX, offY) = moveOffset move
                       in (x + offX, y + offY)
                  )
                  allMoves
              partitionNeighbours point =
                List.partition (`Set.member` blackTiles) $
                  neighbours point

              (currentBlackNeighbours, currentWhiteNeighbours) =
                partitionNeighbours current

              newBlacks =
                mapMaybe
                  ( \whiteTile ->
                      let blackCount = length $ fst $ partitionNeighbours whiteTile
                       in if blackCount == 2
                            then Just whiteTile
                            else Nothing
                  )
                  currentWhiteNeighbours
           in newBlacks
                ++ if null currentBlackNeighbours
                  || length currentBlackNeighbours > 2
                  then []
                  else [current]
      )
      blackTiles

moveOffset :: Move -> (Float, Float)
moveOffset move =
  case move of
    East ->
      (1, 0)
    SouthEast ->
      (0.5, -1)
    SouthWest ->
      (-0.5, -1)
    West ->
      (-1, 0)
    NorthEast ->
      (0.5, 1)
    NorthWest ->
      (-0.5, 1)

movesPosition :: [Move] -> (Float, Float)
movesPosition =
  List.foldl'
    ( \(accX, accY) move ->
        let (moveX, moveY) = moveOffset move
         in (accX + moveX, accY + moveY)
    )
    (0, 0)

-- parsers

inputParser :: Parser [[Move]]
inputParser = do
  P.sepEndBy1 (P.many1 moveParser) P.newline

moveParser :: Parser Move
moveParser = do
  P.choice
    [ East <$ P.char 'e',
      West <$ P.char 'w',
      do
        P.char 's'
        P.choice
          [ SouthWest <$ P.char 'w',
            SouthEast <$ P.char 'e'
          ],
      do
        P.char 'n'
        P.choice
          [ NorthWest <$ P.char 'w',
            NorthEast <$ P.char 'e'
          ]
    ]
