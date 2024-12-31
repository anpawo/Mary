{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Compiler bytecode
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
-- to prevent warnings
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Eta reduce" #-}

module Bytecode.Compiler
  (
    -- testCompiler,
    --main
    compiler,
    Instruction(..),
    Value(..),
    EnvVar(..),

    --test
    convertLiteral,
    compileSubExpression,
    compileExpression,
    compileExpressions,
    compileParam,
    compileParams
  ) where

import Ast.Ast
import Parser.Token (Literal(..), Type(..))

data Instruction
  = Push Value
  | Call
  | Ret
  | Store String
  | Load String
  | PushEnv String
  deriving (Show, Eq)

data Value
  = VmChar Char
  | VmBool Bool
  | VmInt Int
  | VmFloat Double
  | VmString String
  deriving (Show, Eq)

data EnvVar = EnvVar { envVarName :: String, envVarBody :: [Instruction]} deriving (Show, Eq)

convertLiteral :: Literal -> Value
convertLiteral (CharLit c) = VmChar c
convertLiteral (BoolLit b) = VmBool b
convertLiteral (IntLit i) = VmInt i
convertLiteral (FloatLit f) = VmFloat f
convertLiteral (StringLit s) = VmString s

compileSubExpression :: SubExpression -> [Instruction]
compileSubExpression (VariableCall varName) = [Load varName]
compileSubExpression (FunctionCall fnName args) = concatMap compileSubExpression args ++ [PushEnv fnName, Call]
compileSubExpression (Literal lit) = [Push (convertLiteral lit)]

compileExpression :: Expression -> [Instruction]
compileExpression (SubExpression subExpr) = compileSubExpression subExpr
compileExpression (Variable (_, name) value) = compileSubExpression value ++ [Store name]
compileExpression (Return value) = compileSubExpression value ++ [Ret]

compileExpressions :: [Expression] -> [Instruction]
compileExpressions = concatMap compileExpression

compileParam :: (Type, String) -> [Instruction]
compileParam (_, paramName) = [Store paramName]

compileParams :: [(Type, String)] -> [Instruction]
compileParams params = concatMap compileParam (reverse params)

astToEnvVar :: Ast -> Either String EnvVar
astToEnvVar (Function fnName fnArgs _ fnBody) = Right $ EnvVar fnName (compileParams fnArgs ++ compileExpressions fnBody)
astToEnvVar (Operator opName _ _ opArgLeft opArgRight opBody) = Right $ EnvVar opName (compileParams [opArgLeft,opArgRight] ++ compileExpressions opBody)
astToEnvVar other = Left $ "Unsupported AST to bytecode: " ++ show other

findMainFunc :: [EnvVar] -> Bool
findMainFunc envVars = any (\envVar -> envVarName envVar == "main") envVars

compiler :: [Ast] -> Either String ([Instruction], [EnvVar])
compiler asts =
  case mapM astToEnvVar asts of
    Left err -> Left err
    Right envVars -> if findMainFunc envVars
      then Right ([PushEnv "main", Call], envVars)
      else Right ([], envVars)

-- testCompiler :: IO ()
-- testCompiler = do
--   -- let asts = [Function {
--   --     fnName = "add_mul",
--   --     fnArgs = [
--   --         (IntType, "a"),
--   --         (IntType, "b"),
--   --         (IntType, "c")
--   --     ],
--   --     fnRetType = IntType,
--   --     fnBody = [
--   --       Return {
--   --         retValue = FunctionCall {
--   --             fnName = "+",
--   --             fnArgs = [
--   --                 VariableCall {varName = "a"},
--   --                 FunctionCall {
--   --                     fnName = "*",
--   --                     fnArgs = [ VariableCall {varName = "b"}, VariableCall {varName = "c"} ]
--   --                 }
--   --             ]
--   --         }
--   --       }
--   --     ]
--   --   }, Function {
--   --     fnName = "main",
--   --     fnArgs = [],
--   --     fnRetType = IntType,
--   --     fnBody = [
--   --       SubExpression FunctionCall { fnName = "add_mul", fnArgs = [Literal (IntLit 1), Literal (IntLit 2), Literal (IntLit 3)]},
--   --       Return {retValue = Literal (IntLit 0)}
--   --     ]
--   --   }]
--   let asts = [Operator {opName = "**", opPrecedence = 8, opRetType = IntType, opArgLeft = (IntType,"n"), opArgRight = (IntType,"power"), opBody = [Return {retValue = FunctionCall {fnName = "**", fnArgs = [VariableCall {varName = "n"},FunctionCall {fnName = "-", fnArgs = [VariableCall {varName = "power"},Literal (IntLit 1)]}]}}]},Function {fnName = "test", fnArgs = [], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnName = "**", fnArgs = [Literal (IntLit 2),Literal (IntLit 3)]}}]}]
--   print $ compiler asts