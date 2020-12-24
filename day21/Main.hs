{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.IO as TI
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case P.parse (P.sepEndBy1 foodParser P.newline) "" contents of
    Left err ->
      print err
    Right foods -> do
      let summary = buildAllergenSummary foods
      print summary
      let allergenIngredients = mconcat $ HM.elems summary
      print $
        length $
          filter (not . (`Set.member` allergenIngredients)) $
            concatMap ingredients foods
      let ingredientsWithAllergens = findIngredientsWithAllergens summary
      print $
        List.intercalate "," $
          map snd $ List.sort ingredientsWithAllergens

data Food = Food
  { ingredients :: [String],
    allergens :: [String]
  }
  deriving (Show)

buildAllergenSummary :: [Food] -> HM.HashMap String (Set.Set String)
buildAllergenSummary =
  List.foldl'
    ( \acc Food {ingredients, allergens} ->
        let ingSet = Set.fromList ingredients
         in List.foldl'
              ( \innerAcc all ->
                  HM.insertWith Set.intersection all ingSet innerAcc
              )
              acc
              allergens
    )
    HM.empty

findIngredientsWithAllergens :: HM.HashMap String (Set.Set String) -> [(String, String)]
findIngredientsWithAllergens summary =
  findResult (sortIngredients $ HM.toList summary) []
  where
    sortIngredients = List.sortOn (length . snd)
    findResult [] acc = acc
    findResult ((firstIng, firstAllergens) : rest) acc =
      let firstAllergen = head $ Set.toList firstAllergens
          newRest = map (second (Set.delete firstAllergen)) rest
       in findResult (sortIngredients newRest) ((firstIng, firstAllergen) : acc)

foodParser :: Parser Food
foodParser = do
  ingredients <- P.sepEndBy1 (P.many1 P.letter) (P.char ' ')
  Food ingredients <$> P.option [] allergensParser
  where
    allergensParser = do
      P.string "(contains "
      allergens <- P.sepBy1 (P.many1 P.letter) (P.string ", ")
      P.char ')'
      return allergens
