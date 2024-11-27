{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}
{-# LANGUAGE InstanceSigs #-}

module AST
    ( sexprToAST,
        evalAST,
        AST(..),
        Define,
    ) where

import SExprParser
import Control.Applicative()

data Define = Define { name :: String, value :: AST } deriving (Show)

data Function = Function { func_name :: String, args :: [Either String AST] } deriving (Show)

data Condition = Condition { condition :: AST, _true :: AST, _false :: AST } deriving (Show)

data AST = AstFunction Function |
    AstDefine Define |
    AstInt Int |
    AstStr String |
    AstBool Bool |
    AstCondition Condition

instance Show AST where
    show :: AST -> String
    show (AstFunction f) = show f
    show (AstDefine d) = show d
    show (AstInt n) = show n
    show (AstStr s) = show s
    show (AstBool b) = show b
    show (AstCondition c) = show c

instance Eq AST where
    (==) :: AST -> AST -> Bool
    (AstInt x) == (AstInt y) = x == y
    (AstStr str1) == (AstStr str2) = str1 == str2
    (AstBool bool1) == (AstBool bool2) = bool1 == bool2
    _ == _ = False

sexprToASTList :: [SExpr] -> [Either String AST]
sexprToASTList (x : xs) = sexprToAST x : sexprToASTList xs
sexprToASTList [] = []

isValidCondition :: AST -> Bool
isValidCondition (AstBool _) = True
isValidCondition (AstFunction _) = True
isValidCondition _ = False

sexprToASTCondition :: SExpr -> SExpr -> SExpr -> Either String AST
sexprToASTCondition _condition _true _false =
    case (sexprToAST _condition, sexprToAST _true, sexprToAST _false) of
        (Right _condition, Right _true, Right _false) ->
            if isValidCondition _condition
                then Right (AstCondition (Condition _condition _true _false))
                else Left "Error in if condition: Condition must be a boolean or a function"
        (Left err, _, _) -> Left ("Error in if condition: " ++ err)
        (_, Left err, _) -> Left ("Error in if true branch: " ++ err)
        (_, _, Left err) -> Left ("Error in if false branch: " ++ err)

sexprToAST :: SExpr -> Either String AST
sexprToAST (SExprAtomInt num) = Right (AstInt num)
sexprToAST (SExprAtomString "#t") = Right (AstBool True)
sexprToAST (SExprAtomString "#f") = Right (AstBool False)
sexprToAST (SExprAtomString str) = Right (AstStr str)
sexprToAST (SExprList [SExprAtomString "define", SExprAtomString _name, _value]) =
    case sexprToAST _value of
        Right astValue -> Right (AstDefine (Define _name astValue))
        Left err -> Left ("Error in define value: " ++ err)
sexprToAST (SExprList [SExprAtomString "if", _condition, _true, _false]) = sexprToASTCondition _condition _true _false
sexprToAST (SExprList (SExprAtomString _name : args)) =
    let parsedArgs = sexprToASTList args
     in Right (AstFunction (Function _name parsedArgs))
sexprToAST _ = Left "Unrecognized SExpr"

checkFunction :: String -> [Either String AST] -> Either String [AST]
checkFunction _name args =
    case sequence args of
        Right [AstInt x, AstInt y] -> Right [AstInt x, AstInt y]
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

checkDivid :: String -> [Either String AST] -> Either String [AST]
checkDivid _name args =
    case sequence args of
        Right [AstInt _, AstInt 0] -> Left "Division by 0 is forbidden"
        Right [AstInt x, AstInt y] -> Right [AstInt x, AstInt y]
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

checkBool :: String -> [Either String AST] -> Either String [AST]
checkBool _name args =
    case sequence args of
        Right [x, y] -> Right [x, y]
        Right [AstDefine _, _] -> Left "A define type cannot be compared"
        Right [AstFunction _, _] -> Left "A function type cannot be compared"
        Right [_, AstDefine _] -> Left "A define type cannot be compared"
        Right [_, AstFunction _] -> Left "A function type cannot be compared"
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

evalAST :: [Define] -> AST -> Either String ([Define], AST)
evalAST list_define (AstInt num) = Right (list_define, AstInt num)
evalAST list_define (AstBool True) = Right (list_define, AstBool True)
evalAST list_define (AstBool False) = Right (list_define, AstBool False)
evalAST list_define (AstStr str) = Right (list_define, AstStr str)
    -- case valueEither of
    --     Right value -> evalAST list_define value >>= \(list_define, evaluatedValue) ->
    --         Right (list_define, AstDefine (Define _name (Right evaluatedValue)))
    --     Left err -> Left ("Error evaluating define value: " ++ err)

evalAST list_define (AstCondition (Condition {condition = cond, _true = _t, _false = _f})) = case evalAST list_define cond of
    Right (list_define, AstBool True) -> evalAST list_define _t
    Right (list_define, AstBool False) -> evalAST list_define _f
    Right _ -> Left "Error evaluating the AST: a condition is required to return a bool"
    Left err -> Left err
evalAST list_define (AstDefine def) = Right (list_define ++ [def], AstDefine def)
evalAST list_define (AstFunction (Function {func_name = "+", args = args})) =
    checkFunction "+" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstInt (x + y))
evalAST list_define (AstFunction (Function {func_name = "*", args = args})) =
    checkFunction "*" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstInt (x * y))
evalAST list_define (AstFunction (Function {func_name = "-", args = args})) =
    checkFunction "-" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstInt (x - y))
evalAST list_define (AstFunction (Function {func_name = "<", args = args})) =
    checkFunction "<" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstBool (x < y))
evalAST list_define (AstFunction (Function {func_name = "div", args = args})) =
    checkDivid "div" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstInt (x `div` y))
evalAST list_define (AstFunction (Function {func_name = "mod", args = args})) =
    checkDivid "mod" args >>= \[AstInt x, AstInt y] ->
        Right (list_define, AstInt (x `mod` y))
evalAST list_define (AstFunction (Function {func_name = "eq?", args = args})) =
    checkBool "eq?" args >>= \[x, y] ->
        Right (list_define, AstBool (x == y))
evalAST _ _ = Left "Error evaluating the AST"