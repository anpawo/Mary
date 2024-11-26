{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}

module AST
    ( sexprToAST,
        evalAST,
        AST(..),
    ) where

import SExprParser
import Control.Applicative()

data Define = Define { name :: String, value :: Either String AST } deriving (Show)

data Function = Function { func_name :: String, args :: [Either String AST] } deriving (Show)

data Condition = Condition { condition :: AST, _true :: AST, _false :: AST } deriving (Show)

data AST = AstFunction Function |
    AstDefine Define |
    AstInt Int |
    AstStr String |
    AstBool Bool |
    AstCondition Condition

instance Show AST where
    show (AstFunction f) = show f
    show (AstDefine d) = show d
    show (AstInt n) = show n
    show (AstStr s) = show s
    show (AstBool b) = show b
    show (AstCondition c) = show c

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
        (Right condition, Right _true, Right _false) ->
            if isValidCondition condition
                then Right (AstCondition (Condition condition _true _false))
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
        Right astValue -> Right (AstDefine (Define _name (Right astValue)))
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
        Right [AstInt x, AstInt 0] -> Left "Division by 0 is forbidden"
        Right [AstInt x, AstInt y] -> Right [AstInt x, AstInt y]
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

evalAST :: AST -> Either String AST
evalAST (AstInt num) = Right (AstInt num)
evalAST (AstBool True) = Right (AstBool True)
evalAST (AstBool False) = Right (AstBool False)
evalAST (AstStr str) = Right (AstStr str)
evalAST (AstCondition (Condition {condition = cond, _true = _t, _false = _f})) = case evalAST cond of
    Right (AstBool True) -> evalAST _t
    Right (AstBool False) -> evalAST _f
    Right _ -> Left "Error evaluating the AST: a condition is required to return a bool"
    Left err -> Left err
evalAST (AstDefine (Define {name = _name, value = valueEither})) =
    case valueEither of
        Right value -> evalAST value >>= \evaluatedValue ->
            Right (AstDefine (Define _name (Right evaluatedValue)))
        Left err -> Left ("Error evaluating define value: " ++ err)
evalAST (AstFunction (Function {func_name = "+", args = args})) =
    checkFunction "+" args >>= \[AstInt x, AstInt y] ->
        Right (AstInt (x + y))
evalAST (AstFunction (Function {func_name = "*", args = args})) =
    checkFunction "*" args >>= \[AstInt x, AstInt y] ->
        Right (AstInt (x * y))
evalAST (AstFunction (Function {func_name = "-", args = args})) =
    checkFunction "-" args >>= \[AstInt x, AstInt y] ->
        Right (AstInt (x - y))
evalAST (AstFunction (Function {func_name = "div", args = args})) =
    checkDivid "div" args >>= \[AstInt x, AstInt y] ->
        Right (AstInt (x `div` y))
evalAST (AstFunction (Function {func_name = "mod", args = args})) =
    checkDivid "mod" args >>= \[AstInt x, AstInt y] ->
        Right (AstInt (x `mod` y))
evalAST _ = Left "Error evaluating the AST"