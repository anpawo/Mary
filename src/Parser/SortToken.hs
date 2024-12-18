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
    (insideStruct, CurlyClose: restAfterStruct) -> do
          let (linesList, _) = splitBySemicolon insideStruct
          restTokens <- sortToken restAfterStruct
          Right (StructureSort name linesList : restTokens)
    _ -> Left "error: invalid structure format"
sortTokenStruct _ = Left "error: invalid struct format"

sortTokenFunct :: [Token] -> Either String [Token]
sortTokenFunct (SymbolId name : ParenOpen : rest) =
  case span (/= ParenClose) rest of
    (params, ParenClose : Arrow : functionType :CurlyOpen : bodyTokens) ->
      let (linesList, restAfterParam) = splitBySemicolon bodyTokens
      in if head restAfterParam == CurlyClose
         then do
           let (sortParams, _) = splitByComma params
           restTokens <- sortToken (drop 1 restAfterParam)
           Right (FunctionSort name sortParams functionType linesList : restTokens)
         else Left "error: missing or extra tokens after closing curly brace"
    _ -> Left "error: invalid function format"
sortTokenFunct _ = Left "error: invalid function format"

sortToken :: [Token] -> Either String [Token]
sortToken [] = Right []
sortToken (FunctionKw : function) = sortTokenFunct function
sortToken (StructKw : structure) = sortTokenStruct structure
sortToken _ = Left "error: invalid file format"