{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Parser.SortToken
    (
        splitBySemicolon,
        sortToken
    ) where


import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Parser.SortTokenData
import Parser.Token

findLine :: [Token] -> [Token] -> ([SortToken], [Token])
findLine [] [] = ([], [])
findLine [] lines = ([Line (reverse lines)], [])
findLine (CurlyClose:rest) [] = ([], CurlyClose:rest)
findLine (CurlyClose:rest) lines = ([Line (reverse lines)], CurlyClose:rest)
findLine (SemiColon:rest) lines = 
  let (new_line, remaining) = findLine rest [] 
  in (Line (reverse lines) : new_line, remaining)
findLine (t:rest) lines = findLine rest (t:lines)

splitBySemicolon :: [Token] -> ([SortToken], [Token])
splitBySemicolon [] = ([], [])
splitBySemicolon tokens = findLine tokens []

sortTokenStruct :: [Token] -> Either String [SortToken]
sortTokenStruct (SymbolId name: CurlyOpen : rest) =
  case span (/= CurlyClose) rest of
    (inside_struct, CurlyClose: rest_after_struct) -> do
          let (lines, _) = splitBySemicolon inside_struct
          restTokens <- sortToken rest_after_struct
          Right (Structure {struct_name = name, struct_body = lines} : restTokens)
    _ -> Left "error: invalid structure format"
sortTokenStruct _ = Left "error: invalid struct format"

sortTokenFunct :: [Token] -> Either String [SortToken]
sortTokenFunct (SymbolId name : ParenOpen : rest) =
  case span (/= ParenClose) rest of
    (params, ParenClose : Arrow : function_type :CurlyOpen : bodyTokens) ->
      let (lines, rest_after_param) = splitBySemicolon bodyTokens
      in if head rest_after_param == CurlyClose
         then do
           restTokens <- sortToken (drop 1 rest_after_param)
           Right (Function {func_name = name, func_param = params, func_type = function_type, func_body = lines} : restTokens)
         else Left "error: missing or extra tokens after closing curly brace"
    _ -> Left "error: invalid function format"
sortTokenFunct _ = Left "error: invalid function format"

sortToken :: [Token] -> Either String [SortToken]
sortToken [] = Right []
sortToken (FunctionKw : function) = sortTokenFunct function
sortToken (StructKw : structure) = sortTokenStruct structure
sortToken _ = Left "error: invalid file format"