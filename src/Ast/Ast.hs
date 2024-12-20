{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ast.Ast (Ast(..), Expression (..), tokenToAst) where
import Parser.Token
    ( Token(StructKw, FunctionKw, OperatorKw),
      Identifier(..),
      Literal,
      Type(IntType) )

data Expression
  = VariableCall { varName :: Identifier }
  | FunctionCall { fnName :: Identifier, fnArgs :: [Identifier] }
  | IfThenElse { ifCond :: Expression, thenExpr :: Expression, elseExpr :: Expression }
  | Literal Literal
  | Return { retValue :: Expression }
  | Builtin { builtinName :: Identifier }
  | Variable { varName :: Identifier, fnValue :: Expression }
  deriving (Show)

data Ast
  = Structure { structName :: Identifier, structMember :: [(Type, Identifier)] }
  | Function { fnName :: Identifier, fnRetType :: Type, fnArgs :: [(Type, Identifier)], fnBody :: [Expression] }
  | Operator { opName :: Identifier, opPrecedence :: Int, opRetType :: Type, opArgLeft :: (Type, Identifier), opArgRight :: (Type, Identifier), opBody :: [Expression] }
  deriving (Show)

type Ctx = [Ast]
type AstError = String

builtin :: [Ast]
builtin =
    [ Operator {opName = OperatorId "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, SymbolId "l"), opArgRight = (IntType, SymbolId "r"), opBody = [Builtin $ SymbolId "+"]}
    , Operator {opName = OperatorId "*", opPrecedence = 7, opRetType = IntType, opArgLeft = (IntType, SymbolId "l"), opArgRight = (IntType, SymbolId "r"), opBody = [Builtin $ SymbolId "*"]}
    ]

tokenToAst :: [Token] -> Either AstError [Ast]
tokenToAst = ast builtin

ast :: Ctx -> [Token] -> Either AstError [Ast]
ast ctx [] = Right ctx
ast ctx (FunctionKw: token) = function ctx token >>= uncurry ast
ast ctx (OperatorKw: token) = operator ctx token >>= uncurry ast
ast ctx (StructKw: token) = structure ctx token >>= uncurry ast
ast _ _ = Left ""

function :: Ctx -> [Token] -> Either AstError (Ctx, [Token])
function _ _ = Left "function todo"

operator :: Ctx -> [Token] -> Either AstError (Ctx, [Token])
operator _ _ = Left "operator todo"

structure :: Ctx -> [Token] -> Either AstError (Ctx, [Token])
structure _ _ = Left "structure todo"
