{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

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
    Right (xs, ys) -> do
      -- let result = normalGame xs ys
      -- print result
      -- print $ score $ winner result
      let recursiveResult = recursiveGame xs ys Set.empty
      print recursiveResult
      print $ score $ winner recursiveResult

type Deck = [Int]

data GameResult
  = P1Wins Deck
  | P2Wins Deck
  deriving (Show)

winner :: GameResult -> Deck
winner result =
  case result of
    P1Wins d -> d
    P2Wins d -> d

score :: Deck -> Int
score deck =
  sum $
    zipWith (*) (reverse deck) [1 ..]

normalGame :: Deck -> Deck -> GameResult
normalGame p1 [] = P1Wins p1
normalGame [] p2 = P2Wins p2
normalGame (x : xs) (y : ys) =
  if x > y
    then normalGame (xs <> [x, y]) ys
    else normalGame xs (ys <> [y, x])

recursiveGame :: Deck -> Deck -> Set.Set (Deck, Deck) -> GameResult
recursiveGame p1 [] _history = P1Wins p1
recursiveGame [] p2 _history = P2Wins p2
recursiveGame p1@(x : xs) p2@(y : ys) history
  | (p1, p2) `elem` history = P1Wins p1
  | x <= length xs && y <= length ys = recursiveGameResult
  | x > y = p1WonRound
  | otherwise = p2WonRound
  where
    newHistory = Set.insert (p1, p2) history
    p1WonRound = recursiveGame (xs <> [x, y]) ys newHistory
    p2WonRound = recursiveGame xs (ys <> [y, x]) newHistory
    recursiveGameResult =
      case recursiveGame (take x xs) (take y ys) Set.empty of
        P1Wins _ ->
          p1WonRound
        P2Wins _ ->
          p2WonRound

-- parsers

inputParser :: Parser (Deck, Deck)
inputParser = do
  P.string "Player 1:\n"
  first <- P.sepEndBy1 intParser P.newline
  P.string "\nPlayer 2:\n"
  (first,) <$> P.sepEndBy1 intParser P.newline

intParser :: Parser Int
intParser =
  read <$> P.many1 P.digit
