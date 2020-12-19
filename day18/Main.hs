{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  -- let contents = "1 + 2 * 3 + 4 * 5 + 6"
  contents <- TI.readFile "input.txt"
  let exprs = rights $ map (P.parse expressionParser "") (T.lines contents)
  print $ sum $ map evaluate exprs

data Expression
  = Num Int
  | Add Expression Expression
  | Mul Expression Expression
  deriving (Eq, Show)

evaluate :: Expression -> Int
evaluate expr =
  case expr of
    Num v ->
      v
    Add first second ->
      evaluate first + evaluate second
    Mul first second ->
      evaluate first * evaluate second

-- parser

expressionParser :: Parser Expression
expressionParser = do
  term <- termParser
  expressionHelp [] term

data Operator
  = AddOp
  | MulOp

expressionHelp :: [(Expression, Operator)] -> Expression -> Parser Expression
expressionHelp revOps expr =
  P.choice
    [ do
        P.spaces
        op <- operatorParser
        P.spaces
        newExpr <- termParser
        expressionHelp ((expr, op) : revOps) newExpr,
      return $ finalize revOps expr
    ]

-- finalize :: [(Expression, Operator)] -> Expression -> Expression
-- finalize [] finalExpr = finalExpr
-- finalize ((firstExpr, firstOp) : rest) finalExpr =
--   case firstOp of
--     AddOp ->
--       Add (finalize rest firstExpr) finalExpr
--     MulOp ->
--       Mul (finalize rest firstExpr) finalExpr

finalize :: [(Expression, Operator)] -> Expression -> Expression
finalize [] finalExpr = finalExpr
finalize ((firstExpr, firstOp) : rest) finalExpr =
  case firstOp of
    AddOp ->
      finalize rest (Add firstExpr finalExpr)
    MulOp ->
      Mul (finalize rest firstExpr) finalExpr

operatorParser :: Parser Operator
operatorParser = do
  P.choice
    [ AddOp <$ P.char '+',
      MulOp <$ P.char '*'
    ]

termParser :: Parser Expression
termParser = do
  P.choice
    [ Num . read <$> P.many1 P.digit,
      do
        P.char '('
        P.spaces
        expr <- expressionParser
        P.spaces
        P.char ')'
        return expr
    ]
