{-
-- EPITECH PROJECT, 2024
-- AST.hs
-- File description:
-- glados
-}
{-# LANGUAGE InstanceSigs #-}

module AST.Data
  ( AST (..),
    CheckASTFunc,
    Condition (..),
    Define (..),
    Function (..),
    Lambda (..),
    OpMathFunc,
  )
where

type OpMathFunc = Int -> Int -> Int

type CheckASTFunc = [Define] -> String -> [AST] -> Either String [AST]

data Define = Define {name :: String, value :: AST} deriving (Show)

data Function = Function {func_name :: String, args :: [AST]} deriving (Show)

data Lambda = Lambda {params :: [AST], body :: AST} deriving (Show)

data Condition = Condition {condition :: AST, _true :: AST, _false :: AST} deriving (Show)

data AST
  = AstFunction Function
  | AstLambda Lambda
  | AstDefine Define
  | AstInt Int
  | AstStr String
  | AstBool Bool
  | AstCondition Condition

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