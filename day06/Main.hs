{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let groups = mapMaybe parseGroup (T.splitOn "\n\n" contents)
  print $ sum $ anyoneAnswered <$> groups
  print $ sum $ everyoneAnswered <$> groups

newtype Answer = Answer Char
  deriving (Eq, Ord)

newtype Group = Group {answers :: NonEmpty (Set Answer)}

parseGroup :: T.Text -> Maybe Group
parseGroup input =
  let result = parseAnswers <$> T.lines input
   in case result of
        [] ->
          Nothing
        first : rest ->
          Just $ Group {answers = first :| rest}

parseAnswers :: T.Text -> Set Answer
parseAnswers input =
  Set.fromList $ Answer <$> T.unpack input

anyoneAnswered :: Group -> Int
anyoneAnswered Group {answers} =
  length $ foldl1 Set.union answers

everyoneAnswered :: Group -> Int
everyoneAnswered Group {answers} =
  length $ foldl1 Set.intersection answers
