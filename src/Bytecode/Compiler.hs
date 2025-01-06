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

    --test
    convertLiteral,
    compileSubExpression,
    compileExpression,
    compileExpressions,
    compileParam,
    compileParams,
    findMainFunc
  ) where

import Ast.Ast
import Parser.Token (Literal(..), Type(..))
import Bytecode.Data

convertLitStruct :: (String, SubExpression) -> [(String, [Instruction])]
convertLitStruct (key, value) = [(key , compileSubExpression value)]

convertLiteral :: Literal -> Value
convertLiteral (CharLit c) = VmChar c
convertLiteral (BoolLit b) = VmBool b
convertLiteral (IntLit i) = VmInt i
convertLiteral (FloatLit f) = VmFloat f
convertLiteral (StringLit s) = VmString s
convertLiteral (ArrLit _ arr) = VmArray $ concatMap compileSubExpression arr
convertLiteral (StructLit _ structMember) = VmStruct $ concatMap convertLitStruct structMember
convertLiteral NullLit = VmNull

compileSubExpression :: SubExpression -> [Instruction]
compileSubExpression (VariableCall varName) = [Load varName]
compileSubExpression (FunctionCall fnName args) = concatMap compileSubExpression args ++ [Push $ VmFunc fnName, Call]
compileSubExpression (Lit lit) = [Push (convertLiteral lit)]

compileExpression :: Expression -> [Instruction]
compileExpression (SubExpression subExpr) = compileSubExpression subExpr
compileExpression (Variable (_, name) value) = compileSubExpression value ++ [Store name]
compileExpression (Return value) = compileSubExpression value ++ [Ret]
compileExpression (IfThenElse cond true false) = instructionsCond ++ [JumpIfFalse nbInstructionsTrue] ++ instructionsTrue ++ instructionsFalse
  where
      instructionsTrue = compileExpressions true
      instructionsFalse = compileExpressions false
      instructionsCond = compileSubExpression cond
      nbInstructionsTrue = length instructionsTrue
compileExpression (While cond body) = instructionsCond ++ [JumpIfFalse (nbSkipLoop + 2)] ++ instructionsBody ++ instructionsCond ++ [JumpIfFalse 2] ++ [JumpBackward (nbSkipLoop + 1)]
  where
    instructionsCond = compileSubExpression cond
    instructionsBody = compileExpressions body
    nbSkipLoop = length instructionsCond + length instructionsBody

compileExpressions :: [Expression] -> [Instruction]
compileExpressions = concatMap compileExpression

compileParam :: (Type, String) -> [Instruction]
compileParam (_, paramName) = [Store paramName]

compileParams :: [(Type, String)] -> [Instruction]
compileParams params = concatMap compileParam (reverse params)

astToEnvVar :: Ast -> Either String EnvVar
astToEnvVar (Function fnName fnArgs _ fnBody) = Right   (fnName ,(compileParams fnArgs ++ compileExpressions fnBody))
astToEnvVar (Operator opName _ _ opArgLeft opArgRight opBody) = Right  (opName ,(compileParams [opArgLeft,opArgRight] ++ compileExpressions opBody))
astToEnvVar other = Left $ "Unsupported AST to bytecode: " ++ show other

findMainFunc :: [EnvVar] -> Bool
findMainFunc envVars = any (\(nameFunc, _) ->  nameFunc == "main") envVars

compiler :: [Ast] -> Either String ([Instruction], [EnvVar])
compiler asts =
  case mapM astToEnvVar asts of
    Left err -> Left err
    Right envVars -> if findMainFunc envVars
      then Right ([Push $ VmFunc "main", Call], envVars)
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