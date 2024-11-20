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

data Define = Define {name::String, value::Int} deriving (Show)

data Function = Function {func_name::String, arg::[Ast]} deriving (Show)

data Ast = AstFunction Function | AstDefine Define | AstInt Int | AstStr String | AstBool Bool deriving (Show)

isValid :: Maybe Ast -> Ast
isValid Nothing = AstStr "Error Nothing"
isValid (Just x) = x

sexprToASTList :: [SExpr] -> [Ast]
sexprToASTList (x:xs) = (isValid (sexprToAST x): sexprToASTList xs)
sexprToASTList [] = []

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SExprAtomInt num) = Just (AstInt num)
sexprToAST (SExprAtomString "#t") = Just (AstBool True)
sexprToAST (SExprAtomString "#f") = Just (AstBool False)
sexprToAST (SExprAtomString str) = Just (AstStr str)
sexprToAST (SExprList [SExprAtomString "define", SExprAtomString _name, SExprAtomInt _value]) = Just (AstDefine (Define _name _value))
sexprToAST (SExprList (SExprAtomString x : xs)) = Just (AstFunction (Function x (sexprToASTList xs)))
sexprToAST _ = Nothing
