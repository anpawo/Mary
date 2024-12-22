{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Ast.Ast
  (
    -- main
    Ast(..),
    tokenToAst,

    -- test
    runParser,
    function
  ) where

import Text.Megaparsec (Parsec, single, eof, satisfy, choice, runParser, MonadParsec(..), setOffset, getOffset)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.Functor (($>))

import Parser.Token (MyToken(..), Identifier(..), Literal(..), Type(..))
import Ast.Error

type Parser = Parsec Void [MyToken]

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
ast ctx = (eof $> []) <|> ((function ctx <|> operator ctx <|> structure <|> failN errTopLevelDef) >>= (\x -> ast (x : ctx)))

tok :: MyToken -> Parser MyToken
tok = single

sym :: Parser String
sym = satisfy isSym >>= (\t -> case t of
  Identifier (SymbolId name) -> pure name
  _ -> fail errImpossibleCase
  )
  where
    isSym (Identifier (SymbolId _)) = True
    isSym _ = False

types :: Bool -> Parser Type
types canBeVoid = choice (t ++ vt) <|> failN (errExpectedType canBeVoid)
  where
    t =
      [ tok (Type CharType) $> CharType
      , tok (Type BoolType) $> BoolType
      , tok (Type IntType) $> IntType
      , tok (Type FloatType) $> FloatType
      , tok (Type StrType) $> StrType
      , tok (Type ArrType) $> ArrType
      ]
    vt
      | canBeVoid = [tok (Type VoidType) $> VoidType]
      | otherwise = []

failN :: (MonadParsec e s m, MonadFail m) => String -> m a
failN err = (setOffset . (+ 1) =<< getOffset) *> fail err

-- The function exists to be able to return the value expected
notTaken :: [String] -> (a -> String) -> a -> Parser a
notTaken names f x
  | name `elem` names = fail $ errNameTaken name
  | otherwise = pure x
  where name = f x

getFnName :: [String] -> Parser String
getFnName names = tok FunctionKw *> sym >>= notTaken names id

getFnArgs :: [String] -> Parser [(Type, String)]
getFnArgs names = tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs names)
  where
    getAllArgs names' = do
      arg <- (,) <$> types False <*> (sym >>= notTaken names' id)
      endFound <- tok ParenClose <|> tok Comma
      case endFound of
        ParenClose -> pure [arg]
        Comma -> (arg :) <$> getAllArgs (snd arg : names')
        _ -> fail errImpossibleCase

-- getFnArgs names = tok ParenOpen *> many ((,) <$> types <*> sym <* (lookAhead (tok ParenClose) <|> tok Comma)) <* tok ParenClose

getFnRetType :: Parser Type
getFnRetType = tok Arrow *> types True

getNames :: Ctx -> [String]
getNames [] = []
getNames (Structure {structName = name}:rst) = name : getNames rst
getNames (Function {fnName = name}:rst) = name : getNames rst
getNames (Operator {opName = name}:rst) = name : getNames rst

-- expression :: Ctx -> Parser Expression
-- expression ctx = fail "expression todo"

-- functionBody :: Ctx -> [(Type, String)] -> Type -> Parser [Expression]
-- functionBody _ _ _ = pure []
-- functionBody ctx args retT = do
--   expr <- notFollowedBy (tok CurlyClose) >> expression
--   expr

function :: Ctx -> Parser Ast
function ctx = do
  name <- getFnName (getNames ctx)
  args <- getFnArgs (name : getNames ctx)
  retT <- getFnRetType
  -- body <- tok CurlyOpen *> functionBody (Function name args retT [] : ctx) args retT <* tok CurlyClose
  -- return $ Function name args retT body
  return $ Function name args retT []

operator :: Ctx -> Parser Ast
operator _ = tok (Type CharType) $> Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
-- temporary

structure :: Parser Ast
structure = tok (Type CharType) $> Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
-- tu peux delete ce qui est au dessus c juste pour pouvoir compiler
