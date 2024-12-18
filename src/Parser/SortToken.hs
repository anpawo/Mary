{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Parser.SortToken
    (
      sortToken
    ) where


import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Parser.Token

findParams :: [Token] -> [Token] -> ([Token], [Token])
findParams [] [] = ([], [])
findParams [] linesList = ([Line (reverse linesList)], [])
findParams (Comma:rest) linesList = 
  let (new_line, remaining) = findParams rest [] 
  in (Line (reverse linesList) : new_line, remaining)
findParams (t:rest) linesList = findParams rest (t:linesList)

splitByComma :: [Token] -> ([Token], [Token])
splitByComma [] = ([], [])
splitByComma tokens = findParams tokens []

findLine :: [Token] -> [Token] -> ([Token], [Token])
findLine [] [] = ([], [])
findLine [] linesList = ([Line (reverse linesList)], [])
findLine (CurlyClose:rest) [] = ([], CurlyClose:rest)
findLine (CurlyClose:rest) linesList = ([Line (reverse linesList)], CurlyClose:rest)
findLine (SemiColon:rest) linesList = 
  let (new_line, remaining) = findLine rest [] 
  in (Line (reverse linesList) : new_line, remaining)
findLine (t:rest) linesList = findLine rest (t:linesList)

splitBySemicolon :: [Token] -> ([Token], [Token])
splitBySemicolon [] = ([], [])
splitBySemicolon tokens = findLine tokens []

sortTokenStruct :: [Token] -> Either String [Token]
sortTokenStruct (SymbolId name: CurlyOpen : rest) =
  case span (/= CurlyClose) rest of
    (inside_struct, CurlyClose: rest_after_struct) -> do
          let (linesList, _) = splitBySemicolon inside_struct
          restTokens <- sortToken rest_after_struct
          Right (StructureSort name linesList : restTokens)
    _ -> Left "error: invalid structure format"
sortTokenStruct _ = Left "error: invalid struct format"

sortTokenFunct :: [Token] -> Either String [Token]
sortTokenFunct (SymbolId name : ParenOpen : rest) =
  case span (/= ParenClose) rest of
    (params, ParenClose : Arrow : function_type :CurlyOpen : bodyTokens) ->
      let (linesList, rest_after_param) = splitBySemicolon bodyTokens
      in if head rest_after_param == CurlyClose
         then do
           let (sort_params, _) = splitByComma params
           restTokens <- sortToken (drop 1 rest_after_param)
           Right (FunctionSort name sort_params function_type linesList : restTokens)
         else Left "error: missing or extra tokens after closing curly brace"
    _ -> Left "error: invalid function format"
sortTokenFunct _ = Left "error: invalid function format"

sortToken :: [Token] -> Either String [Token]
sortToken [] = Right []
sortToken (FunctionKw : function) = sortTokenFunct function
sortToken (StructKw : structure) = sortTokenStruct structure
sortToken _ = Left "error: invalid file format"