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
        findDefine,
        AST(..),
        Define(..),
        Function(..),
        Condition(..)
    ) where

import SExprParser
import Control.Applicative()

type OpMathFunc = Int -> Int -> Int

type CheckASTFunc = [Define] -> String -> [AST] -> Either String [AST]

data Define = Define { name :: String, value :: AST } deriving (Show)

data Function = Function { func_name :: String, args :: [AST] } deriving (Show)

data Lambda = Lambda { nb_args :: Int , body :: AST } deriving (Show)

data Condition = Condition { condition :: AST, _true :: AST, _false :: AST } deriving (Show)

data AST = AstFunction Function |
    AstLambda Lambda |
    AstDefine Define |
    AstInt Int |
    AstStr String |
    AstBool Bool |
    AstCondition Condition

instance Show AST where
    show :: AST -> String
    show (AstLambda l) = show l
    show (AstFunction f) = show f
    show (AstDefine d) = show d
    show (AstInt n) = show n
    show (AstStr s) = show s
    show (AstBool True) = "#t"
    show (AstBool False) = "#f"
    show (AstCondition c) = show c

instance Eq AST where
    (==) :: AST -> AST -> Bool
    (AstInt x) == (AstInt y) = x == y
    (AstStr str1) == (AstStr str2) = str1 == str2
    (AstBool bool1) == (AstBool bool2) = bool1 == bool2
    _ == _ = False

sexprToASTList :: [SExpr] -> Either String [AST]
sexprToASTList [] = Right []
sexprToASTList (x : xs) =
    case sexprToAST x of
        Right ast ->
            case sexprToASTList xs of
                Right astList -> Right (ast : astList)
                Left err -> Left err
        Left err -> Left ("Error in list element: " ++ err)

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

changeValLambda :: AST -> [AST] -> AST
changeValLambda (AstLambda (Lambda nb_args (AstFunction (Function _name parsedBody)))) new_param = (AstLambda (Lambda nb_args (AstFunction (Function _name new_param))))

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
        Right astValue -> Right (AstDefine (Define _name astValue))
        Left err -> Left ("Error in define value: " ++ err)
sexprToAST (SExprList [SExprAtomString "if", _condition, _true, _false]) = sexprToASTCondition _condition _true _false
sexprToAST (SExprList [SExprAtomString "lambda", SExprList params, SExprList body]) =
    case mapM extractParam params of
        Just paramList -> case sexprToAST (SExprList body) of
            Right (AstFunction parsedBody) ->
                if length paramList == countFunctionArgs (AstFunction parsedBody)
                    then Right (AstLambda (Lambda (length paramList) (AstFunction parsedBody)))
                    else Left "Error: Number of lambda arguments does not match the number of arguments in the body"
            Right _ -> Left "Lambda body must be a func"
            Left err -> Left ("Error in lambda body: " ++ err)
        Nothing -> Left "Error parsing lambda parameters"
sexprToAST (SExprList (SExprAtomString _name : args)) = case sexprToASTList args of
    Right parsedArgs -> Right (AstFunction (Function _name parsedArgs))
    Left err -> Left ("Error in function parse args" ++ err)
sexprToAST (SExprList (SExprList lambda : values)) = case sexprToAST (SExprList lambda) of
    Right (AstLambda (Lambda nb_args (AstFunction (Function _name parsedBody)))) ->
        if length values == nb_args
                then case sexprToASTList values of
                    Right parsedValues -> Right (changeValLambda (AstLambda (Lambda nb_args (AstFunction (Function _name parsedBody)))) parsedValues) 
                    Left err -> Left ("Error in parsing values: " ++ err)
                else Left "Error: Number of arguments provided does not match the lambda argument count"
    Left err -> Left err
sexprToAST _ = Left "Unrecognized SExpr"

checkFunction :: [Define] -> String -> [AST] -> Either String [AST]
checkFunction _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkFunction list_define _name [x , y] = case evalAST list_define x of
    Right (_, AstInt val_x) -> case evalAST list_define y of
        Right (_, AstInt val_y) -> Right [AstInt val_x, AstInt val_y]
        Left _ -> Left ("Bad number or type of arguments for " ++ _name)
    Left _ -> Left ("Bad number or type of arguments for " ++ _name) 
checkFunction _ _name _ = Left ("Bad number or type of arguments for " ++ _name)

checkDivid :: [Define] -> String -> [AST] -> Either String [AST]
checkDivid _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkDivid list_define _name [x , y] = case evalAST list_define x of
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

findDefine :: [Define] -> String -> Either String AST
findDefine list_define nameToFind =
    case filter (\def -> name def == nameToFind) list_define of
        (Define _ defineValue : _) -> Right defineValue
        [] -> Left ("Define with name '" ++ nameToFind ++ "' not found")

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
    Right value -> evalAST list_define (changeValLambda value args)
evalAST _ _ = Left "Error evaluating the AST"