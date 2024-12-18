{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}

module Parser.SortTokenData (SortToken (..)) where
import Parser.Token

data SortToken
  =
      Line [Token]
    | Function {func_name :: String, func_param:: [Token], func_type :: Token, func_body :: [SortToken]}
    | Structure {struct_name :: String, struct_body :: [SortToken]}
  deriving (Show, Eq)
