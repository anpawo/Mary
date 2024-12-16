{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}

module AST.Tools
    ( sexprToAST,
        findDefine,
        sexprToASTList,
        isValidCondition,
        countFunctionArgs,
        extractParam,
        replaceParam,
        changeValLambda,
        sexprToASTCondition,
        sexprToASTDefine,
        sexprToASTLambdaWithVal,
        sexprToASTLambda,
        checkFunction,
        checkBool,
        checkDivid,
        AST(..),
        OpMathFunc(),
        CheckASTFunc(),
        Define(..),
        Function(..),
        Lambda(..),
        Condition(..)
    ) where

import SExprParser
import Control.Applicative()
import AST.Data
-- import AST.Eval
-- import AST.Check

isValidCondition :: AST -> Bool
isValidCondition (AstBool _) = True
isValidCondition (AstFunction _) = True
isValidCondition _ = False

countFunctionArgs :: AST -> Int
countFunctionArgs (AstFunction (Function _ args)) = length args
countFunctionArgs _ = 0

extractParam :: SExpr -> Maybe AST
extractParam (SExprAtomString param) = Just (AstStr param)
extractParam _ = Nothing

replaceParam :: AST -> AST -> AST -> AST
replaceParam target param value
  | target == param = value
  | otherwise = case target of
      AstFunction (Function name args) -> AstFunction (Function name (map (\arg -> replaceParam arg param value) args))
      AstLambda (Lambda params body) -> AstLambda (Lambda params (replaceParam body param value))
      AstCondition (Condition cond _t _f) -> AstCondition
        (Condition (replaceParam cond param value) (replaceParam _t param value) (replaceParam _f param value))
      _ -> target

changeValLambda :: AST -> [AST] -> Either String AST
changeValLambda (AstLambda (Lambda params body)) newParams =
  if length params == length newParams
    then Right (foldl (\acc (param, value) -> replaceParam acc param value) body (zip params newParams))
    else Left "Error: Number of parameters and values do not match"
changeValLambda ast _ = Right ast
