{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let [rulesContents, messagesContents] = T.splitOn "\n\n" contents
  case P.parse rulesParser "" rulesContents of
    Left err ->
      print err
    Right rules -> do
      let messageParser = buildMessageParser rules
      print $
        length $
          rights $
            map
              (P.parse messageParser "")
              (T.lines messagesContents)
      let messageParser2 = buildMessageParser2 rules
      print $
        length $
          rights $
            map
              (P.parse messageParser2 "")
              (T.lines messagesContents)

data Rule
  = Single Char
  | Sequence [Int]
  | Alternative [Int] [Int]
  deriving (Show)

type Rules = HM.HashMap Int Rule

buildMessageParser :: Rules -> Parser String
buildMessageParser rules = do
  v <- buildForIndex rules 0
  P.eof
  return v

-- rule 0 is 8, 11
-- 8 is 42 | 42, 8
-- 11 is 42, 31 | 42, 11, 31
-- so 0 is 42+ 31+ as long as there are more 42s than 31s
buildMessageParser2 :: Rules -> Parser String
buildMessageParser2 rules = do
  let p42 = buildForIndex rules 42
  let p31 = buildForIndex rules 31
  r42 <- P.many1 (P.try p42)
  r31 <- P.many1 p31
  P.eof
  if length r42 > length r31
    then return (mconcat r42 <> mconcat r31)
    else fail "wrong"

buildForIndex :: Rules -> Int -> Parser String
buildForIndex rules index = do
  case rules ! index of
    Single v ->
      return <$> P.char v
    Sequence group ->
      mconcat <$> mapM (buildForIndex rules) group
    Alternative first second ->
      P.try (mconcat <$> mapM (buildForIndex rules) first)
        <|> (mconcat <$> mapM (buildForIndex rules) second)

-- parsers

rulesParser :: Parser Rules
rulesParser = do
  HM.fromList <$> P.sepEndBy ruleParser P.newline
  where
    ruleParser :: Parser (Int, Rule)
    ruleParser = do
      index <- intParser
      P.string ": "
      rule <-
        P.choice
          [ do
              P.char '"'
              l <- P.letter
              P.char '"'
              return (Single l),
            P.try
              ( do
                  first <- sequenceParser
                  P.string "| "
                  Alternative first <$> sequenceParser
              ),
            Sequence <$> sequenceParser
          ]
      return (index, rule)

    sequenceParser :: Parser [Int]
    sequenceParser = do
      P.sepEndBy1 intParser (P.char ' ')

intParser :: Parser Int
intParser =
  read <$> P.many1 P.digit
