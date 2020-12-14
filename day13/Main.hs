{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse scheduleParser "" contents of
    Left _err ->
      print "Could not parse schedule"
    Right schedule -> do
      let (busId, arrival) = findFirstArrival schedule
      print $ (arrival - earliestTimestamp schedule) * busId
      print $ findScatteredArrivals schedule

data BusReq
  = RunningEvery Integer
  | NoRequirement
  deriving (Show)

data Schedule = Schedule
  { earliestTimestamp :: Integer,
    runningBuses :: [BusReq]
  }
  deriving (Show)

findFirstArrival :: Schedule -> (Integer, Integer)
findFirstArrival Schedule {earliestTimestamp, runningBuses} =
  let busIds = mapMaybe getBusId runningBuses
   in head
        [ (busId, time) | time <- [earliestTimestamp ..], busId <- busIds, mod time busId == 0
        ]

findScatteredArrivals :: Schedule -> Integer
findScatteredArrivals Schedule {runningBuses} =
  chineseRemainder $ modReqs runningBuses
  where
    modReqs :: [BusReq] -> [(Integer, Integer)]
    modReqs busReqs =
      mapMaybe
        ( \(req, index) ->
            (\id -> (id - index, id))
              <$> getBusId req
        )
        (zip busReqs [0 ..])

{-
Let's take the "17,x,13,19 => 3417" example. This means these are valid:

  N mod 17 = 0
  (N + 2) mod 13 = 0
  (N + 3) mod 19 = 0

which is the same as writing

  N mod 17 = 0
  N mod 13 = 11
  N mod 19 = 16

The problem is that normally we have an easy way to get a mod, but not an
easy way to invert a mod!

Math to the rescue: https://en.wikipedia.org/wiki/Modular_multiplicative_inverse

Later in the same wiki page (Application section), they refer to a chinese remainder
theorem which seems to be exactly what we need!

https://en.wikipedia.org/wiki/Chinese_remainder_theorem
https://www.youtube.com/watch?v=ru7mWZJlRQg

For a system with only two rules like these

  x mod m1 = a
  x mod m2 = b

We can also write them as

  x = m1 * k1 + a
  x = m2 * k2 + b

Since we have x in both sides, we can rewrite as

  m1 * k1 + a = m2 * k2 + b
  m1 * k1 - m2 * k2 = b - a
  m1 * (k1/(b - a)) + m2 * (-k2/(b - a)) = 1

If m1 and m2 are coprime, their greatest common divisor will be 1

  gcd(m1, m2) = 1

And using the extended euclidean algorithm will give us u and v such that

  u * m1 + v * m2 = 1

Now we know that

  u = k1 / (b - a)
  k1 = u * (b - a)

  v = -k2 / (b - a)
  k2 = -v (b - a)
  k2 = v (a - b)

We can calculate X as

  x = u * (b − a) * m1 + a
  x = v * (a - b) * m2 + b
-}
chineseRemainder :: [(Integer, Integer)] -> Integer
chineseRemainder elems =
  fst $ List.foldl' go (0, 1) elems
  where
    go (a, m1) (b, m2) = (mod r m, m)
      where
        r = invMod m2 m1 * (a - b) * m2 + b
        m = m2 * m1

{- This answers the question: which number x will satisfy the equation

  a * x = 1 (in modulo b)

Also known as a modular multiplicative inverse. We calculate it using
the extended euclidean algorithm: given two numbers a and b, it will
compute their greatest common divisor, plus two coefficients x and y so that

  a * x + b * y = gcd(a, b)

If those numbers are coprime, their gcd will be 1, so we'll have

  a * x + b * y = 1

Which we can rewrite as

  a * x - 1 = -y * b

Which proves that

  a * x = 1 (in modulo b)
-}
invMod :: Integer -> Integer -> Integer
invMod a m =
  case extendedEuclidean a m of
    (1, i, _) ->
      mod i m
    _ ->
      error (show a <> " and " <> show m <> " are not coprime")

extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean 0 b = (b, 0, 1)
extendedEuclidean a b = (gcd, t - count * s, s)
  where
    (count, remainder) = divMod b a
    (gcd, s, t) = extendedEuclidean remainder a

getBusId :: BusReq -> Maybe Integer
getBusId req =
  case req of
    NoRequirement ->
      Nothing
    RunningEvery v ->
      Just v

scheduleParser :: Parser Schedule
scheduleParser = do
  firstValue <- intParser
  P.spaces
  busIds <- intOrX `P.sepBy` P.char ','
  return $ Schedule firstValue busIds
  where
    intParser :: Parser Integer
    intParser = do
      read <$> P.many1 P.digit

    intOrX :: Parser BusReq
    intOrX = do
      P.choice
        [ NoRequirement <$ P.char 'x',
          RunningEvery <$> intParser
        ]
