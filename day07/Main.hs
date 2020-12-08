{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Graph.Connectivity (areConnected)
import qualified Data.Graph.DGraph as DGraph
import qualified Data.Graph.Types as Graph
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parseLine = P.parse ruleParser ""
  let rules = rights $ map parseLine (T.lines contents)
  let graph =
        DGraph.fromArcsList $
          concatMap
            ( \(name, contains) ->
                map
                  ( \(howMany, contained) ->
                      Graph.Arc name contained howMany
                  )
                  contains
            )
            rules
  print $
    pred $
      length $
        filter
          (\source -> areConnected graph source "shiny gold")
          (Graph.vertices graph)
  print $
    countEachBagContained graph "shiny gold"

ruleParser :: Parser (String, [(Int, String)])
ruleParser = do
  bag <- bagParser
  P.spaces
  P.string "contain"
  P.spaces
  contains <- containsParser
  P.spaces
  P.char '.'
  return $ (,) bag contains

bagParser :: Parser String
bagParser = do
  first <- P.many1 P.letter
  P.spaces
  second <- P.many1 P.letter
  P.spaces
  P.string "bag"
  P.optional (P.char 's')
  return $ first <> " " <> second

containsParser :: Parser [(Int, String)]
containsParser =
  let noOtherBags = do
        P.string "no other bags"
        return []

      quantifiedBag = do
        P.spaces
        value <- P.many1 P.digit
        P.spaces
        contained <- bagParser
        return (read value, contained)

      someOtherBags = do
        P.sepBy quantifiedBag (P.char ',')
   in P.choice [noOtherBags, someOtherBags]

countEachBagContained :: DGraph.DGraph String Int -> String -> Int
countEachBagContained graph fromVertex =
  sum $
    map (\(_, _, i) -> i) $
      go (Graph.reachableAdjacentVertices' graph fromVertex) []
  where
    go [] popped = reverse popped
    go (first@(_, vertex, edge) : rest) popped =
      let result =
            map (\(s, d, e) -> (s, d, edge * e)) $
              Graph.reachableAdjacentVertices' graph vertex
       in go
            (rest ++ result)
            (first : popped)
