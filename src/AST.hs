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
import Control.Applicative

data Define = Define {name::String, value::Int} deriving (Show)

data Ast = AstDefine Define | AstInt Int | AstStr String | AstBool Bool deriving (Show)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SExprAtomInt num) = Just (AstInt num)
sexprToAST (SExprAtomString "#t") = Just (AstBool True)
sexprToAST (SExprAtomString "#f") = Just (AstBool False)
sexprToAST (SExprAtomString str) = Just (AstStr str)
sexprToAST _ = Nothing