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
{-# LANGUAGE RecordWildCards #-}

module Bytecode.Compiler
  (
    --main
    compiler,

    --test
    convertLiteral,
    compileSubExpression,
    compileExpression,
    compileExpressions,
    compileParam,
    compileParams,
    astToEnvVar,
    findMainFunc
  ) where

import Ast.Ast
import Parser.Token (Literal(..), Type(..))
import Bytecode.Data
import Data.Either (rights)

convertLitStruct :: (String, SubExpression) -> [(String, [Instruction])]
convertLitStruct (key, value) = [(key , compileSubExpression value)]

convertLiteral :: Literal -> Value
convertLiteral (ClosureLit name argsTy retTy) = VmClosure name
convertLiteral (CharLit c) = VmChar c
convertLiteral (BoolLit b) = VmBool b
convertLiteral (IntLit i) = VmInt i
convertLiteral (FloatLit f) = VmFloat f
convertLiteral (StringLit s) = VmString s
convertLiteral (ArrLit t arr) = VmPreArray (show t) $ map compileSubExpression arr
convertLiteral (StructLit name structMember) = VmPreStruct name $ concatMap convertLitStruct structMember
convertLiteral NullLit = VmNull
convertLiteral (LambdaLit {..}) = VmPreLambda lambdaCapture $ compileParams lambdaArgs ++ compileSubExpression lambdaBody

compileSubExpression :: SubExpression -> [Instruction]
compileSubExpression (VariableCall varName) = [Load varName]
compileSubExpression (FunctionCall "." [VariableCall varCallName, nameField]) = [Push (VmString varCallName)] ++ res ++ [Push (VmFunc "."), Call]
  where
    res = compileSubExpression nameField
compileSubExpression (FunctionCall fnName args) = concatMap compileSubExpression args ++ [Push $ VmFunc fnName, Call]
compileSubExpression (Lit lit) = [Push (convertLiteral lit)]

compileExpression :: Expression -> [Instruction]
compileExpression (SubExpression subExpr) = compileSubExpression subExpr
compileExpression (StructField name field value) = [Push $ VmString field] ++ compileSubExpression value ++ [Update name]
compileExpression (Variable (_, name) value) = compileSubExpression value ++ [Store name]
compileExpression (Return value) = compileSubExpression value ++ [Ret]
compileExpression (IfThenElse cond true false) = instructionsCond ++ [JumpIfFalse (nbInstructionsTrue + 2)] ++ instructionsTrue ++ [Push (VmBool False), JumpIfFalse nbInstructionsFalse] ++instructionsFalse
  where
    instructionsTrue = compileExpressions true
    instructionsFalse = compileExpressions false
    instructionsCond = compileSubExpression cond
    nbInstructionsTrue = length instructionsTrue
    nbInstructionsFalse = length instructionsFalse
compileExpression (While cond body) = instructionsCond ++ [JumpIfFalse (nbSkipLoop + 2)] ++ instructionsBody ++ instructionsCond ++ [JumpIfFalse 1] ++ [JumpBackward (nbSkipLoop + 1)]
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
astToEnvVar (Function fnName fnArgs _ fnBody) = Right (fnName, compileParams fnArgs ++ compileExpressions fnBody)
astToEnvVar (Operator opName _ _ opArgLeft opArgRight opBody) = Right (opName, compileParams [opArgLeft,opArgRight] ++ compileExpressions opBody)
astToEnvVar other = Left $ "Unsupported AST to bytecode: " ++ show other

findMainFunc :: [EnvVar] -> Bool
findMainFunc envVars = any (\(nameFunc, _) ->  nameFunc == "main") envVars

compiler :: [Ast] -> Either String ([Instruction], [EnvVar])
compiler asts = let envVars = rights $ map astToEnvVar asts
  in if findMainFunc envVars
    then Right ([Push $ VmFunc "main", Call], envVars)
    else Right ([], envVars)
