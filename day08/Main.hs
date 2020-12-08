{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parseOp = P.parse opParser ""
  case traverse parseOp (T.lines contents) of
    Left errs ->
      print errs
    Right ops -> do
      let program = Vector.fromList ops
      let initial = Context 0 0 Set.empty
      print $ stopBeforeInfiniteLoop initial program
      print $ rights $ stopBeforeInfiniteLoop initial <$> permuteNopAndJump program

data Op
  = Acc Int
  | Jump Int
  | NoOp Int
  deriving (Show)

type Program = Vector Op

data Context = Context
  { programCounter :: Int,
    registerValue :: Int,
    opsUsed :: Set.Set Int
  }

permuteNopAndJump :: Program -> [Program]
permuteNopAndJump program =
  Vector.ifoldl'
    ( \acc index elem ->
        case elem of
          NoOp v ->
            program // [(index, Jump v)] : acc
          Jump v ->
            program // [(index, NoOp v)] : acc
          Acc _ ->
            acc
    )
    [program]
    program

stopBeforeInfiniteLoop :: Context -> Program -> Either Int Int
stopBeforeInfiniteLoop context@Context {programCounter, registerValue, opsUsed} program =
  case (programCounter == length program, result) of
    (True, _) ->
      Right registerValue
    (False, Just newContext) ->
      stopBeforeInfiniteLoop newContext program
    (False, Nothing) ->
      Left registerValue
  where
    op = program ! programCounter
    result =
      if Set.member programCounter opsUsed
        then Nothing
        else Just $ runOp op (context {opsUsed = Set.insert programCounter opsUsed})
    runOp op newContext =
      case op of
        NoOp _ ->
          newContext {programCounter = programCounter + 1}
        Acc value ->
          newContext
            { programCounter = programCounter + 1,
              registerValue = registerValue + value
            }
        Jump value ->
          newContext {programCounter = programCounter + value}

-- Parsers

signedNumParser :: Parser Int
signedNumParser = do
  sign <- P.choice [P.char '-', P.char '+']
  value <- P.many1 P.digit
  let result = read value
  return $
    case sign of
      '-' -> negate result
      _ -> result

opParser :: Parser Op
opParser = do
  let accParser = do
        P.string "acc"
        P.spaces
        Acc <$> signedNumParser

      jmpParser = do
        P.string "jmp"
        P.spaces
        Jump <$> signedNumParser

      nopParser = do
        P.string "nop"
        P.spaces
        NoOp <$> signedNumParser
   in P.choice
        [ accParser,
          jmpParser,
          nopParser
        ]
