{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ast.Ast
  (
    -- main
    Ast(..),
    tokenToAst,

    -- test
    runParser,
    function
  ) where

import Text.Megaparsec (Parsec, single, eof, satisfy, choice, many, MonadParsec (lookAhead), runParser, anySingle)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.Functor (($>))

import Parser.Token (Token(..), Identifier(..), Literal(..), Type(..))

type Parser = Parsec Void [Token]

data SubExpression
  = VariableCall { varName :: String }
  | FunctionCall { fnName :: String, fnArgs :: [SubExpression] }
  | Literal Literal
  | Builtin { builtinName :: String } -- operator + - * /
  deriving (Show, Eq)

data Expression
  = SubExpression SubExpression
  | Variable { varName :: String, fnValue :: SubExpression } -- variable creation inside a function
  | Return { retValue :: SubExpression }
  | IfThenElse { ifCond :: SubExpression, thenExpr :: Expression, elseExpr :: Expression }
  deriving (Show, Eq)

data Ast
  = Structure { structName :: String, structMember :: [(Type, String)] }
  | Function { fnName :: String, fnArgs :: [(Type, String)], fnRetType :: Type, fnBody :: [Expression] }
  | Operator { opName :: String, opPrecedence :: Int, opRetType :: Type, opArgLeft :: (Type, String), opArgRight :: (Type, String), opBody :: [Expression] }
  deriving (Show, Eq)

type Ctx = [Ast]

builtin :: [Ast]
builtin =
    [ Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
    , Operator {opName = "*", opPrecedence = 7, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "*"]}
    ]

tokenToAst :: Parser Ctx
tokenToAst = ast builtin

ast :: Ctx -> Parser Ctx
ast ctx = (eof $> []) <|> ((function ctx <|> operator ctx <|> structure) >>= (\x -> ast (x : ctx)))

tok :: Token -> Parser Token
tok = single

sym :: Parser String
sym = satisfy isSym >>= (\(Identifier (SymbolId name)) -> pure name)
  where
    isSym (Identifier (SymbolId _)) = True
    isSym _ = False

types :: Parser Type
types = choice
  [ tok (Type CharType) $> CharType
  , tok (Type VoidType) $> VoidType
  , tok (Type BoolType) $> BoolType
  , tok (Type IntType) $> IntType
  , tok (Type FloatType) $> FloatType
  , tok (Type StrType) $> StrType
  , tok (Type ArrType) $> ArrType
  ]

argsDef :: Parser [(Type, String)]
argsDef = tok ParenOpen *> many ((,) <$> types <*> sym <* (lookAhead (tok ParenClose) <|> tok Comma)) <* tok ParenClose

retType :: Parser Type
retType = tok Arrow *> types

expression :: Ctx -> Parser Expression
expression _ = anySingle $> SubExpression (Builtin "test")

function :: Ctx -> Parser Ast
function ctx = Function <$> (tok FunctionKw *> sym) <*> argsDef <*> retType <*> {- validate body before anything -} many (expression ctx) -- validate function

operator :: Ctx -> Parser Ast
operator _ = pure $ Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}

structure :: Parser Ast
structure = pure $ Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
-- tu peux delete ce qui est au dessus c juste pour pouvoir compiler
