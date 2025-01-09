{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ExpressionParser
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ast.ExpressionParser (expression) where

import Ast.Ast
import Parser.Token
import Utils.Lib
import Ast.Error
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Text.Megaparsec (getOffset, choice, some, try)
import Ast.TokenParser
import Control.Monad (void)
import Control.Applicative ((<|>))
import Ast.TreeBuilder

expression :: Ctx -> LocalVariable -> Type -> Parser Expression
expression ctx locVar retT = choice
  [ exprReturn ctx locVar retT
  , exprVariable ctx locVar
  , exprSubexpr ctx locVar retT
  , exprIf ctx locVar retT
  , exprWhile ctx locVar retT
  ]

getBlock :: Ctx -> LocalVariable -> Type -> Parser [Expression]
getBlock ctx locVar retT = tok CurlyOpen *> some (expression ctx locVar retT) <* tok CurlyClose

exprIf :: Ctx -> LocalVariable -> Type -> Parser Expression
exprIf ctx locVar retT = do
  start <- (+1) <$> (tok IfKw *> getOffset)
  cond <- subexpression ctx locVar (tok ThenKw)
  getType ctx locVar cond >>= \case
    t
      | t /= BoolType -> (getOffset >>= (failI start . errCondNotBool) . subtract start)
      | otherwise     -> IfThenElse cond <$> getBlock ctx locVar retT <*> ((tok ElseKw *> getBlock ctx locVar retT) <|> pure [])

exprWhile :: Ctx -> LocalVariable -> Type -> Parser Expression
exprWhile ctx locVar retT = do
  start <- (+1) <$> (tok WhileKw *> getOffset)
  cond <- subexpression ctx locVar (tok ThenKw)
  getType ctx locVar cond >>= \case
    t
      | t == BoolType -> While cond <$> getBlock ctx locVar retT
      | otherwise     -> (getOffset >>= (failI start . errCondNotBool) . subtract start)

exprReturn :: Ctx -> LocalVariable -> Type -> Parser Expression
exprReturn _ _ VoidType = failN errVoidRet
exprReturn ctx locVar retT = tok ReturnKw *> getOffset >>= \offset -> subexpression ctx locVar (tok SemiColon) >>= \subexpr ->
  case subexpr of
    (VariableCall x) -> case fromJust $ find (\(_, n) -> n == x) locVar of
        (t, _)
          | t == retT -> return $ Return subexpr
          | otherwise -> failI offset $ errRetType (show retT) (show t)
    (FunctionCall {fnCallName = name}) -> case fromJust $ find (\a -> (isOp a || isFn a) && getName a == name) ctx of
        (Operator {..})
          | opRetType == retT -> return $ Return subexpr
          | otherwise -> failI offset $ errRetType (show retT) (show opRetType)
        (Function {..})
          | fnRetType == retT -> return $ Return subexpr
          | otherwise -> failI offset $ errRetType (show retT) (show fnRetType)
        _ -> failN $ errImpossibleCase "exprReturn function call"
    (Lit x)
      | getLitType x == retT -> return $ Return subexpr
      | otherwise -> failI offset $ errRetType (show retT) (show $ getLitType x)

exprVariable :: Ctx -> LocalVariable -> Parser Expression
exprVariable ctx locVar = variableCreation ctx locVar <|> variableAssignation ctx locVar

variableCreation :: Ctx -> LocalVariable -> Parser Expression
variableCreation ctx locVar = do
  t <- types ctx False True
  n <- textIdentifier
  void (tok Assign)
  x <- subexpression ctx locVar (tok SemiColon)
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failP $ errAssignType n (show t) (show t')

variableAssignation :: Ctx -> LocalVariable -> Parser Expression
variableAssignation ctx locVar = do
  n <- try $ textIdentifier <* tok Assign
  t <- case find ((== n) . snd) locVar of
    Nothing -> fail $ errVariableNotBound n
    Just (t, _) -> pure t
  x <- subexpression ctx locVar (tok SemiColon)
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failP $ errAssignType n (show t) (show t')

exprSubexpr :: Ctx -> LocalVariable -> Type -> Parser Expression
exprSubexpr ctx locVar _ = SubExpression <$> subexpression ctx locVar (tok SemiColon)
