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
exprReturn ctx locVar retT = tok ReturnKw *> case retT of
  VoidType -> failN errVoidRet
  _ -> getOffset >>= \offset -> subexpression ctx locVar (tok SemiColon) >>= \subexpr ->
    case subexpr of
      (VariableCall x) -> case fromJust $ find (\(_, n) -> n == x) locVar of
          (t, _)
            | t == retT -> return $ Return subexpr
            | otherwise -> failI offset $ errRetType (show retT) (show t)
      (FunctionCall {fnCallName = name}) -> case find (\a -> (isOp a || isFn a) && getName a == name) ctx of
          Just (Operator {..})
            | opRetType == retT -> return $ Return subexpr
            | otherwise -> failI offset $ errRetType (show retT) (show opRetType)
          Just (Function {..})
            | fnRetType == retT -> return $ Return subexpr
            | otherwise -> failI offset $ errRetType (show retT) (show fnRetType)
          Nothing -> case fromJust $ find ((== name) . snd) locVar of
            (ClosureType _ closureRetType, _)
              | closureRetType == retT -> return $ Return subexpr
              | otherwise -> failI offset $ errRetType (show retT) (show closureRetType)
            _ -> failN $ errImpossibleCase "exprReturn function call"
          _ -> failN $ errImpossibleCase "exprReturn function call"
      (Lit x)
        | getLitType x == retT -> return $ Return subexpr
        | otherwise -> failI offset $ errRetType (show retT) (show $ getLitType x)

exprVariable :: Ctx -> LocalVariable -> Parser Expression
exprVariable ctx locVar = variableCreation ctx locVar <|> variableAssignation ctx locVar <|> modifyStructField ctx locVar

variableCreation :: Ctx -> LocalVariable -> Parser Expression
variableCreation ctx locVar = do
  n <- try $ textIdentifier <* tok Colon
  t <- types ctx False True
  offset <- getOffset <* tok Assign
  x <- subexpression ctx locVar (tok SemiColon)
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failI offset $ errAssignType n (show t) (show t')

variableAssignation :: Ctx -> LocalVariable -> Parser Expression
variableAssignation ctx locVar = do
  (n, isCompound) <- try $ (,) <$> textIdentifier <*> (tok Assign <|> tok AssignAdd <|> tok AssignSub <|> tok AssignMul <|> tok AssignDiv)
  offset <- getOffset
  t <- case find ((== n) . snd) locVar of
    Nothing -> fail $ errVariableNotBound n
    Just (t, _) -> pure t
  x <- subexpression ctx locVar (tok SemiColon) >>= \e -> case isCompound of
    Assign -> pure e
    AssignAdd -> pure $ FunctionCall "+" [VariableCall n, e] 
    AssignSub -> pure $ FunctionCall "-" [VariableCall n, e] 
    AssignMul -> pure $ FunctionCall "*" [VariableCall n, e] 
    AssignDiv -> pure $ FunctionCall "/" [VariableCall n, e] 
    _ -> fail $ errImpossibleCase "compound assign"
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failI offset $ errAssignType n (show t) (show t')

modifyStructField :: Ctx -> LocalVariable -> Parser Expression
modifyStructField ctx locVar = try $ do
  varName <- textIdentifier
  case find ((== varName) . snd) locVar of
    Just (_, _) -> pure ()
    _ -> fail $ errVariableNotBound varName
  structField <- ((\op -> if op == "." then pure () else failP errExpectedField :: Parser ()) <$> operatorIdentifier) *> textIdentifier <* tok Assign
  subexpr <- subexpression ctx locVar (tok SemiColon)
  return $ StructField varName structField subexpr

exprSubexpr :: Ctx -> LocalVariable -> Type -> Parser Expression
exprSubexpr ctx locVar _ = SubExpression <$> subexpression ctx locVar (tok SemiColon)
