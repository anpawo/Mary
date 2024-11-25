{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}

module AST
    ( sexprToAST
    ) where

import SExprParser
import Control.Applicative()

data Define = Define {name::String, value::Maybe Ast} deriving (Show)

data Function = Function {func_name::String, arg::[Maybe Ast]} deriving (Show)

data Ast = AstFunction Function | AstDefine Define | AstInt Int | AstStr String | AstBool Bool deriving (Show)

sexprToASTList :: [SExpr] -> [Maybe Ast]
sexprToASTList (x : xs) = (sexprToAST x: sexprToASTList xs)
sexprToASTList [] = []

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SExprAtomInt num) = Just (AstInt num)
sexprToAST (SExprAtomString "#t") = Just (AstBool True)
sexprToAST (SExprAtomString "#f") = Just (AstBool False)
sexprToAST (SExprAtomString str) = Just (AstStr str)
sexprToAST (SExprList [SExprAtomString "define", SExprAtomString _name, _value]) = Just (AstDefine (Define _name (sexprToAST _value)))
sexprToAST (SExprList (SExprAtomString _name : args)) = Just (AstFunction (Function _name (sexprToASTList args)))
sexprToAST _ = Nothing
