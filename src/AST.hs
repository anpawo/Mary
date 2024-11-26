{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}

module AST
    ( sexprToAST,
      evalAST
    ) where

import SExprParser
import Control.Applicative()

data Define = Define { name :: String, value :: Either String Ast } deriving (Show)

data Function = Function { func_name :: String, args :: [Either String Ast] } deriving (Show)

data Ast = AstFunction Function | AstDefine Define | AstInt Int | AstStr String | AstBool Bool deriving (Show)

instance Eq Ast where
    (AstInt x) == (AstInt y) = x == y
    (AstStr str1) == (AstStr str2) = str1 == str2
    (AstBool bool1) == (AstBool bool2) = bool1 == bool2
    _ == _ = False

sexprToASTList :: [SExpr] -> [Either String Ast]
sexprToASTList (x : xs) = sexprToAST x : sexprToASTList xs
sexprToASTList [] = []

sexprToAST :: SExpr -> Either String Ast
sexprToAST (SExprAtomInt num) = Right (AstInt num)
sexprToAST (SExprAtomString "#t") = Right (AstBool True)
sexprToAST (SExprAtomString "#f") = Right (AstBool False)
sexprToAST (SExprAtomString str) = Right (AstStr str)
sexprToAST (SExprList [SExprAtomString "define", SExprAtomString _name, _value]) =
    case sexprToAST _value of
        Right astValue -> Right (AstDefine (Define _name (Right astValue)))
        Left err -> Left ("Error in define value: " ++ err)
sexprToAST (SExprList (SExprAtomString _name : args)) =
    let parsedArgs = sexprToASTList args
     in Right (AstFunction (Function _name parsedArgs))
sexprToAST _ = Left "Unrecognized SExpr"

checkFunction :: String -> [Either String Ast] -> Either String [Ast]
checkFunction _name args =
    case sequence args of
        Right [AstInt x, AstInt y] -> Right [AstInt x, AstInt y]
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

checkDivid :: String -> [Either String Ast] -> Either String [Ast]
checkDivid _name args =
    case sequence args of
        Right [AstInt _, AstInt 0] -> Left "Division by 0 is forbidden"
        Right [AstInt x, AstInt y] -> Right [AstInt x, AstInt y]
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

checkBool :: String -> [Either String Ast] -> Either String [Ast]
checkBool _name args =
    case sequence args of
        Right [x, y] -> Right [x, y]
        Right [AstDefine _, _] -> Left "A define type cannot be compared"
        Right [AstFunction _, _] -> Left "A function type cannot be compared"
        Right [_, AstDefine _] -> Left "A define type cannot be compared"
        Right [_, AstFunction _] -> Left "A function type cannot be compared"
        Right _ -> Left ("Bad number or type of arguments for " ++ _name)
        Left err -> Left ("Error parsing arguments: " ++ err)

evalAST :: Ast -> Either String Ast
evalAST (AstInt num) = Right (AstInt num)
evalAST (AstBool True) = Right (AstBool True)
evalAST (AstBool False) = Right (AstBool False)
evalAST (AstStr str) = Right (AstStr str)
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
evalAST (AstFunction (Function {func_name = "eq?", args = args})) =
    checkBool "eq?" args >>= \[x, y] ->
        Right (AstBool (x == y))
evalAST _ = Left "Error evaluating the Ast"