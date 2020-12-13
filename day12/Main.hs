{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parseLine = P.parse actionParser ""
  let actions = rights $ map parseLine (T.lines contents)

  let initialContext = Context (Pos 0 0) (Degree 90)
  let result = foldl runAction initialContext actions
  print $ manhattan (pos initialContext) (pos result)

  let waypointContext = WaypointContext (Pos 0 0) (Pos 10 1)
  let result' = foldl runWaypointAction waypointContext actions
  print $ manhattan (shipPos waypointContext) (shipPos result')

data Pos = Pos
  { getX :: Float,
    getY :: Float
  }
  deriving (Eq, Show)

newtype Degree = Degree Float deriving (Eq, Show, Num)

data Context = Context
  { pos :: Pos,
    facing :: Degree
  }
  deriving (Eq, Show)

data WaypointContext = WaypointContext
  { shipPos :: Pos,
    waypointPos :: Pos
  }
  deriving (Eq, Show)

data Action
  = GoNorth Float
  | GoSouth Float
  | GoEast Float
  | GoWest Float
  | GoForward Float
  | TurnLeft Degree
  | TurnRight Degree
  deriving (Eq, Show)

runAction :: Context -> Action -> Context
runAction context@Context {pos, facing} action =
  case action of
    GoNorth v ->
      context {pos = Pos (getX pos) (getY pos + v)}
    GoSouth v ->
      context {pos = Pos (getX pos) (getY pos - v)}
    GoEast v ->
      context {pos = Pos (getX pos + v) (getY pos)}
    GoWest v ->
      context {pos = Pos (getX pos - v) (getY pos)}
    GoForward v ->
      let newX = getX pos + roundTwoDigits (sin (toRad facing)) * v
          newY = getY pos + roundTwoDigits (cos (toRad facing)) * v
       in context {pos = Pos newX newY}
    TurnRight (Degree v) ->
      let (Degree degrees) = facing
       in context {facing = Degree (degrees + v)}
    TurnLeft (Degree v) ->
      let (Degree degrees) = facing
       in context {facing = Degree (degrees - v)}

toRad :: Degree -> Float
toRad (Degree degrees) =
  degrees * pi / 180

roundTwoDigits :: Float -> Float
roundTwoDigits n =
  (/ 100) $ fromInteger $ round $ n * 100

runWaypointAction :: WaypointContext -> Action -> WaypointContext
runWaypointAction context@WaypointContext {shipPos, waypointPos} action =
  case action of
    GoNorth v ->
      context {waypointPos = Pos (getX waypointPos) (getY waypointPos + v)}
    GoSouth v ->
      context {waypointPos = Pos (getX waypointPos) (getY waypointPos - v)}
    GoEast v ->
      context {waypointPos = Pos (getX waypointPos + v) (getY waypointPos)}
    GoWest v ->
      context {waypointPos = Pos (getX waypointPos - v) (getY waypointPos)}
    GoForward v ->
      let newX = getX shipPos + (getX waypointPos * v)
          newY = getY shipPos + (getY waypointPos * v)
       in context {shipPos = Pos newX newY}
    TurnRight degrees ->
      context {waypointPos = rotatePos waypointPos (- degrees)}
    TurnLeft degrees ->
      context {waypointPos = rotatePos waypointPos degrees}
  where
    rotatePos aPos deg =
      let angleCos = roundTwoDigits (cos (toRad deg))
          angleSin = roundTwoDigits (sin (toRad deg))
          newX = getX aPos * angleCos - getY aPos * angleSin
          newY = getX aPos * angleSin + getY aPos * angleCos
       in Pos newX newY

manhattan :: Pos -> Pos -> Float
manhattan first second =
  abs (getX first - getX second)
    + abs (getY first - getY second)

actionParser :: Parser Action
actionParser =
  P.choice
    [ do
        P.char 'N'
        GoNorth <$> num,
      do
        P.char 'S'
        GoSouth <$> num,
      do
        P.char 'E'
        GoEast <$> num,
      do
        P.char 'W'
        GoWest <$> num,
      do
        P.char 'F'
        GoForward <$> num,
      do
        P.char 'L'
        TurnLeft . Degree <$> num,
      do
        P.char 'R'
        TurnRight . Degree <$> num
    ]
  where
    num :: Parser Float
    num = read <$> P.many1 P.digit
