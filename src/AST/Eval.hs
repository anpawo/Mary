{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Eval
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AST.Eval
  ( evalAST,
    evalOpMathFunc,
    evalASTCondition,
    checkFunction,
    checkDivid,
    checkBool,
    findDefine,
  )
where

import AST.Data
  ( AST (..),
    CheckASTFunc,
    Condition (..),
    Define (..),
    Function (Function, args, func_name),
    Lambda (Lambda),
    OpMathFunc,
  )
import AST.Tools (changeValLambda)

findDefine :: [Define] -> String -> Either String AST
findDefine list_define nameToFind =
  case filter (\def -> name def == nameToFind) list_define of
    (Define _ defineValue : _) -> Right defineValue
    [] -> Left ("Define with name '" ++ nameToFind ++ "' not found")

checkFunction :: [Define] -> String -> [AST] -> Either String [AST]
checkFunction _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkFunction list_define _name [x, y] = case evalAST list_define x of
  Right (_, AstInt val_x) -> case evalAST list_define y of
    Right (_, AstInt val_y) -> Right [AstInt val_x, AstInt val_y]
    Left _ -> Left ("Bad number or type of arguments for " ++ _name ++ " params: " ++ show x ++ " " ++ show y)
  Left _ -> Left ("Bad number or type of arguments for " ++ _name ++ " params: " ++ show x ++ " " ++ show y)
checkFunction _ _name _ = Left ("Bad number or type of arguments for " ++ _name)

checkDivid :: [Define] -> String -> [AST] -> Either String [AST]
checkDivid _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkDivid list_define _name [x, y] = case evalAST list_define x of
  Right (_, AstInt val_x) -> case evalAST list_define y of
    Right (_, AstInt val_y) -> Right [AstInt val_x, AstInt val_y]
    Right (_, AstInt 0) -> Left "Division by 0 is forbidden"
    Left _ -> Left ("Bad number or type of arguments for " ++ _name)
  Left _ -> Left ("Bad number or type of arguments for " ++ _name)
checkDivid _ _name _ = Left ("Bad number or type of arguments for " ++ _name)

checkBool :: [Define] -> [AST] -> Either String [AST]
checkBool list_define [x, y] = case evalAST list_define x of
  Right (_, val_x) -> case evalAST list_define y of
    Right (_, val_y) -> Right [val_x, val_y]
    Left err -> Left err
  Left err -> Left err
checkBool _ _ = Left "need only 2 arguments"

evalOpMathFunc :: [Define] -> String -> [AST] -> CheckASTFunc -> OpMathFunc -> Either String ([Define], AST)
evalOpMathFunc list_define func_name args checkAstfunc opMathFunc = case checkAstfunc list_define func_name args of
  Left err -> Left err
  Right [AstInt x, AstInt y] -> Right (list_define, AstInt (opMathFunc x y))
  _ -> Left ("Unexpected error in " ++ func_name)

evalASTCondition :: [Define] -> Condition -> Either String ([Define], AST)
evalASTCondition list_define (Condition {condition = cond, _true = _t, _false = _f}) = case evalAST list_define cond of
  Right (list_define, AstBool True) -> evalAST list_define _t
  Right (list_define, AstBool False) -> evalAST list_define _f
  Right _ -> Left "Error evaluating the AST: a condition is required to return a bool"
  Left err -> Left err

evalAST :: [Define] -> AST -> Either String ([Define], AST)
evalAST list_define (AstInt num) = Right (list_define, AstInt num)
evalAST list_define (AstBool True) = Right (list_define, AstBool True)
evalAST list_define (AstBool False) = Right (list_define, AstBool False)
evalAST list_define (AstStr "") = Right (list_define, AstStr "")
evalAST list_define (AstStr str) = case findDefine list_define str of
  Left err -> Left err
  Right value -> evalAST list_define value
evalAST list_define (AstCondition cond) = evalASTCondition list_define cond
evalAST list_define (AstDefine def) = Right (list_define ++ [def], AstDefine def)
evalAST list_define (AstFunction (Function {func_name = "+", args = args})) = evalOpMathFunc list_define "+" args checkFunction (+)
evalAST list_define (AstFunction (Function {func_name = "-", args = args})) = evalOpMathFunc list_define "-" args checkFunction (-)
evalAST list_define (AstFunction (Function {func_name = "*", args = args})) = evalOpMathFunc list_define "*" args checkFunction (*)
evalAST list_define (AstFunction (Function {func_name = "div", args = args})) = evalOpMathFunc list_define "div" args checkDivid div
evalAST list_define (AstFunction (Function {func_name = "mod", args = args})) = evalOpMathFunc list_define "mod" args checkDivid mod
evalAST list_define (AstFunction (Function {func_name = "<", args = args})) = case checkFunction list_define "<" args of
  Left err -> Left err
  Right [AstInt x, AstInt y] -> Right (list_define, AstBool (x < y))
  _ -> Left "Unexpected error in comparison"
evalAST list_define (AstFunction (Function {func_name = "eq?", args = args})) = case checkBool list_define args of
  Left err -> Left err
  Right [x, y] -> Right (list_define, AstBool (x == y))
  _ -> Left "Unexpected error in equality check"
evalAST list_define (AstLambda (Lambda _ func)) = evalAST list_define func
evalAST list_define (AstFunction (Function {func_name = _name, args = args})) = case findDefine list_define _name of
  Left err -> Left err
  Right value -> case changeValLambda value args of
    Right res -> evalAST list_define res
    Left err -> Left err
evalAST _ _ = Left "Error evaluating the AST"
