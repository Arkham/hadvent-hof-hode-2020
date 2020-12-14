{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Bits as Bits
import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let parseLine = P.parse instructionParser ""
  let instructions = rights $ map parseLine (T.lines contents)
  let initialContext = Context Vector.empty HM.empty

  let first = foldl (flip $ runInstruction maskValue) initialContext instructions
  print $ sum $ HM.elems (memory first)

  let second = foldl (flip $ runInstruction maskMemory) initialContext instructions
  print $ sum $ HM.elems (memory second)

data MaskOp
  = UnknownBit
  | SetBit
  | ClearBit
  deriving (Eq)

instance Show MaskOp where
  show UnknownBit = "X"
  show SetBit = "1"
  show ClearBit = "0"

type Mask = Vector MaskOp

data Instruction
  = SetMask Mask
  | SetMemory Int Int
  deriving (Eq, Show)

type Memory = HM.HashMap Int Int

data Context = Context
  { currentMask :: Mask,
    memory :: Memory
  }
  deriving (Show)

runInstruction :: (Int -> Int -> Mask -> Memory -> Memory) -> Instruction -> Context -> Context
runInstruction setMemoryFn instruction context@Context {currentMask, memory} =
  case instruction of
    SetMask mask ->
      context {currentMask = mask}
    SetMemory address value ->
      context {memory = setMemoryFn address value currentMask memory}

maskValue :: Int -> Int -> Mask -> Memory -> Memory
maskValue address value mask memory =
  let maskedValue =
        Vector.ifoldl'
          ( \acc index current ->
              case current of
                UnknownBit ->
                  acc
                SetBit ->
                  Bits.setBit acc index
                ClearBit ->
                  Bits.clearBit acc index
          )
          value
          mask
   in HM.insert address maskedValue memory

{-
- If the bitmask bit is 0, the corresponding memory address bit is unchanged.
- If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
- If the bitmask bit is X, the corresponding memory address bit is floating.

A floating bit is not connected to anything and instead fluctuates
unpredictably. In practice, this means the floating bits will take on all
possible values, potentially causing many memory addresses to be written all at
once!
-}
maskMemory :: Int -> Int -> Mask -> Memory -> Memory
maskMemory address value mask memory =
  let addresses = buildAllAddresses mask address
   in foldl (\acc addr -> HM.insert addr value acc) memory addresses

buildAllAddresses :: Mask -> Int -> [Int]
buildAllAddresses mask address =
  let (setIndexes, floatIndexes) =
        Vector.ifoldl'
          ( \acc@(setAcc, floatAcc) index op ->
              case op of
                UnknownBit ->
                  (setAcc, index : floatAcc)
                SetBit ->
                  (index : setAcc, floatAcc)
                ClearBit ->
                  acc
          )
          ([], [])
          mask

      maskedAddress =
        foldl (flip Bits.setBit) address setIndexes

      permutateFloating [] values = values
      permutateFloating (first : rest) values =
        permutateFloating rest (values ++ map (`Bits.complementBit` first) values)
   in permutateFloating floatIndexes [maskedAddress]

instructionParser :: Parser Instruction
instructionParser = do
  let setMaskParser = do
        P.string "mask"
        P.spaces
        P.char '='
        P.spaces
        ops <-
          P.count 36 $
            P.choice
              [ UnknownBit <$ P.char 'X',
                SetBit <$ P.char '1',
                ClearBit <$ P.char '0'
              ]
        return $
          SetMask $
            Vector.fromList $
              reverse ops

      setMemoryParser = do
        P.string "mem"
        P.char '['
        address <- intParser
        P.char ']'
        P.spaces
        P.char '='
        P.spaces
        SetMemory address <$> intParser
   in P.choice [P.try setMaskParser, setMemoryParser]

intParser :: Parser Int
intParser =
  read <$> P.many1 P.digit
