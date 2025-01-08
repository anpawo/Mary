{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Parser
-}

module Ast.Parser (tokenToAst) where

import Ast.Ast
import Ast.DeclarationParser
import Text.Megaparsec (eof, many)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Utils.Lib
import Ast.Error
import Ast.TokenParser

tokenToAst :: Ctx -> Ctx -> Parser Ctx
tokenToAst builtins imports = drop (length builtins) <$> (many importKw *> ast (builtins ++ imports))

ast :: Ctx -> Parser Ctx
ast ctx = (eof $> ctx) <|> ((function ctx <|> operator ctx <|> structure ctx <|> constraint ctx <|> failN errTopLevelDef) >>= (\x -> ast (ctx ++ [x])))
