{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Text.IO as TI
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse summaryParser "" contents of
    Right summary -> do
      let (errors, validTickets) = findTicketScanningErrors summary
      print $ sum errors
      let ticketValues = buildValues (length $ head validTickets) validTickets
      let rulesWithIndexes = findRulesIndexes (rules summary) ticketValues []
      print $
        product $
          map (\(_, idx) -> myTicket summary ! idx) $
            filter (List.isPrefixOf "departure" . fst) rulesWithIndexes
    Left err ->
      print err

newtype Req = Req (Int, Int)
  deriving (Eq, Show)

data Rule = Rule
  { ruleName :: String,
    reqs :: (Req, Req)
  }
  deriving (Eq, Show)

type Ticket = Vector Int

data Summary = Summary
  { rules :: [Rule],
    myTicket :: Ticket,
    nearbyTickets :: [Ticket]
  }
  deriving (Eq, Show)

buildValues :: Int -> [Ticket] -> Vector [Int]
buildValues length tickets =
  Vector.fromList $
    map
      (\index -> map (! index) tickets)
      [0 .. (length - 1)]

findRulesIndexes :: [Rule] -> Vector [Int] -> [(String, Int)] -> [(String, Int)]
findRulesIndexes [] _values acc = acc
findRulesIndexes (first : rest) values acc =
  let Rule {ruleName, reqs} = first

      (firstReq, secondReq) =
        reqs

      coveredByReq v (Req (startRange, endRange)) =
        v >= startRange && v <= endRange

      matching =
        filter (`List.notElem` map snd acc) $
          Vector.toList $
            Vector.imapMaybe
              ( \index elems ->
                  let allMatching =
                        all
                          ( \elem ->
                              coveredByReq elem firstReq || coveredByReq elem secondReq
                          )
                   in if allMatching elems
                        then Just index
                        else Nothing
              )
              values
   in case matching of
        [one] ->
          findRulesIndexes rest values ((ruleName, one) : acc)
        _ ->
          findRulesIndexes (rest <> [first]) values acc

findTicketScanningErrors :: Summary -> ([Int], [Ticket])
findTicketScanningErrors summary =
  foldl
    ( \(invalidAcc, validAcc) ticket ->
        let invalidFields =
              filter
                ( \field ->
                    not $
                      any
                        ( \Rule {reqs} ->
                            let (firstReq, secondReq) =
                                  reqs
                                coveredByReq (Req (startRange, endRange)) =
                                  field >= startRange && field <= endRange
                             in coveredByReq firstReq || coveredByReq secondReq
                        )
                        (rules summary)
                )
                (Vector.toList ticket)

            newValidAcc =
              case invalidFields of
                [] ->
                  ticket : validAcc
                _ ->
                  validAcc
         in (invalidAcc <> invalidFields, newValidAcc)
    )
    ([], [])
    (nearbyTickets summary)

-- parsers

summaryParser :: Parser Summary
summaryParser = do
  rules <- P.endBy1 ruleParser P.newline
  P.newline
  P.string "your ticket:"
  P.newline
  myTicket <- ticketParser
  P.count 2 P.newline
  P.string "nearby tickets:"
  P.newline
  nearbyTickets <- P.endBy1 ticketParser P.newline
  return $ Summary rules myTicket nearbyTickets

ruleParser :: Parser Rule
ruleParser = do
  name <- P.many1 (P.choice [P.char ' ', P.letter])
  P.string ": "
  firstRange <- rangeParser
  P.string " or "
  secondRange <- rangeParser
  return $ Rule name (Req firstRange, Req secondRange)
  where
    rangeParser = do
      first <- intParser
      P.char '-'
      second <- intParser
      return (first, second)

ticketParser :: Parser Ticket
ticketParser = do
  Vector.fromList <$> P.sepBy1 intParser (P.char ',')

intParser :: Parser Int
intParser = do
  read <$> P.many1 P.digit
