{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text.IO as TI
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse inputParser "" contents of
    Left err ->
      print err
    Right tiles -> do
      let sidesToTiles = sideHashToTiles tiles
      print $
        product $
          map (fst . head) $
            filter (\elems -> length elems == 2) $
              List.groupBy (\(x, _) (y, _) -> x == y) $
                List.sort $
                  mapMaybe
                    ( \case
                        (side, [tile]) ->
                          Just (tile, side)
                        _ ->
                          Nothing
                    )
                    (HM.toList sidesToTiles)

sideHashToTiles :: [Tile] -> HM.HashMap Int [Int]
sideHashToTiles tiles =
  let tileSidesHashes =
        map
          ( \Tile {tileId, tileCells} ->
              ( tileId,
                concatSides $
                  ( \side ->
                      min
                        (sideHash side)
                        (sideHash $ reverse side)
                  )
                    <$> sides tileCells
              )
          )
          tiles
   in foldl
        ( \acc (id, sideHashes) ->
            foldr
              ( HM.alter
                  ( \case
                      Just v ->
                        Just $ id : v
                      Nothing ->
                        Just [id]
                  )
              )
              acc
              sideHashes
        )
        HM.empty
        tileSidesHashes

data Cell
  = Empty
  | Full

instance Show Cell where
  show Empty = "."
  show Full = "#"

data Tile = Tile
  { tileId :: Int,
    tileCells :: [[Cell]]
  }
  deriving (Show)

data Sides a = Sides
  { top :: a,
    right :: a,
    bottom :: a,
    left :: a
  }
  deriving (Show)

instance Functor Sides where
  fmap fn Sides {top, right, bottom, left} =
    Sides
      { top = fn top,
        right = fn right,
        bottom = fn bottom,
        left = fn left
      }

sides :: [[Cell]] -> Sides [Cell]
sides cells =
  Sides
    { top = head cells,
      bottom = last cells,
      left = head transposed,
      right = last transposed
    }
  where
    transposed = List.transpose cells

sideHash :: [Cell] -> Int
sideHash side = go (reverse side)
  where
    go [] = 0
    go (first : rest) =
      ( case first of
          Empty ->
            0
          Full ->
            1
      )
        + 2 * go rest

concatSides :: Sides a -> [a]
concatSides Sides {top, right, bottom, left} =
  [top, right, bottom, left]

-- parsers

sideLength :: Int
sideLength = 10

inputParser :: Parser [Tile]
inputParser = do
  P.sepEndBy1 tileParser P.spaces

tileParser :: Parser Tile
tileParser = do
  P.string "Tile "
  id <- read <$> P.many1 P.digit
  P.string ":\n"
  cells <-
    P.count
      sideLength
      ( do
          v <-
            P.count
              sideLength
              ( Empty <$ P.char '.'
                  <|> Full <$ P.char '#'
              )
          P.newline
          return v
      )
  return $ Tile id cells
