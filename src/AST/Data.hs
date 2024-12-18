{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}
{-# LANGUAGE InstanceSigs #-}

module AST.Data
  ( AST (..)
  )
where

import Parser.Token

data AST =
    AstNull
  | AstInt Int
  | AstChar Char
  | AstFloat Double
  | AstStr String
  | AstBool Bool
  | AstFunction {funcName :: String, funcType :: Token, funcArgs :: [AST]}
  | AstVar {varName :: String, varType:: Token, varValue :: AST}
  | AstStructure {structName :: String, structArgs :: [AST]}
  deriving (Show)

-- instance Show AST where
--   show :: AST -> String
--   show (AstInt i) = show i
--   show (AstChar c) = show c
--   show (AstFloat f) = show f
--   show (AstStr s) = show s
--   show (AstBool True) = "True"
--   show (AstBool False) = "False"
--   show (AstFunction f) = show f
--   show (AstVar v) = show v

-- instance Eq AST where
--   (==) :: AST -> AST -> Bool
--   (AstInt x) == (AstInt y) = x == y
--   (AstChar x) == (AstChar y) = x == y
--   (AstFloat x) == (AstFloat y) = x == y
--   (AstStr str1) == (AstStr str2) = str1 == str2
--   (AstBool bool1) == (AstBool bool2) = bool1 == bool2
--   _ == _ = False