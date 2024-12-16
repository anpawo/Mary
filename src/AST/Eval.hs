{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Eval
-}

module AST.Eval (
    evalAST,
    evalOpMathFunc,
    evalASTCondition
) where

import AST.Data
import AST.Check

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
    Right value -> case (changeValLambda value args) of
        Right res -> evalAST list_define res
        Left err -> Left err
evalAST _ _ = Left "Error evaluating the AST"
