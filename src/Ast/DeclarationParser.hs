{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopLevelDeclaration
-}
{-# LANGUAGE LambdaCase #-}

module Ast.DeclarationParser (getMembers, getFnArgs, getFnBody, validArgNumber, structure, function, operator, constraint) where

import Parser.Token
import Ast.Ast
import Ast.ExpressionParser
import Ast.TokenParser
import Data.Functor (($>))
import Control.Applicative ((<|>), optional)
import Utils.Lib
import Ast.Error
import Text.Megaparsec (getOffset, satisfy)
import Data.Maybe (fromJust)

getMembers :: Ctx -> [String] -> Parser [(String, Type)]
getMembers ctx names = tok CurlyOpen *> (tok CurlyClose $> [] <|> fields names)
  where
    fields n = (,) <$> (textIdentifier >>= notTaken n) <*> (tok Colon *> types ctx False True) >>= \a -> (tok CurlyClose $> [a]) <|> (tok Comma *> ((a :) <$> fields (fst a : n)))

structure :: Ctx -> Parser Ast
structure ctx = do
  name <- structType >>= notTaken (getNames ctx) . stTyName
  let shellStruct = Structure name []
  members <- getMembers (shellStruct : ctx) (name : getNames ctx)
  return $ Structure name members

getFnArgs :: Ctx -> [String] -> Parser [(Type, String)]
getFnArgs ctx names = tok ParenOpen *> (tok ParenClose $> [] <|> args names)
  where
    args n = (\n' t -> (t, n')) <$> (textIdentifier >>= notTaken n) <*> (tok Colon *> types ctx False True) >>= \a -> (tok ParenClose $> [a]) <|> (tok Comma *> ((a :) <$> args (snd a : n)))

function :: Ctx -> Parser Ast
function ctx = do
  isBuiltin <- tok FunctionKw *> optional (tok BuiltinKw)
  name <- textIdentifier >>= notTaken (getNames ctx)
  args <- getFnArgs ctx (name : getNames ctx)
  retT <- (tok Arrow <|> failN errMissingRetT) *> ((satisfy isStructLiteral *> fail errMisingBody) <|> types ctx True True)
  case isBuiltin of
    Just _ -> pure $ Function name args retT []
    Nothing -> Function name args retT <$> getFnBody (Function name args retT [] : ctx) args retT

operator :: Ctx -> Parser Ast
operator ctx = do
  isBuiltin <- tok OperatorKw *> optional (tok BuiltinKw)
  name <- operatorIdentifier >>= notTaken (getNames ctx)
  prcd <- (tok PrecedenceKw *> intLiteral) <|> pure 0
  args <- getOffset >>= (\offset -> getFnArgs ctx (name : getNames ctx) >>= validArgNumber offset name) . (+1)
  retT <- (tok Arrow <|> failN errMissingRetT) *> ((satisfy isStructLiteral *> fail errMisingBody) <|> types ctx True True)
  case isBuiltin of
    Just _ -> pure $ Operator name prcd retT (head args) (args !! 1) []
    Nothing -> Operator name prcd retT (head args) (args !! 1) <$> getFnBody (Operator name prcd retT (head args) (args !! 1) [] : ctx) args retT

constraint :: Ctx -> Parser Ast
constraint ctx = Constraint <$> (constraintType >>= notTaken (getNames ctx) . fromJust . crTyName) <*> (tok Assign *> constrTypes)
  where
    constrTypes = types ctx False False >>= \t -> (tok SemiColon $> [t]) <|> (tok Pipe *> ((t:) <$> constrTypes))

getFnBody :: Ctx -> LocalVariable -> Type -> Parser [Expression]
getFnBody ctx locVar retT = (tok CurlyOpen <|> failN errStartBody) *> getExprAndUpdateCtx ctx locVar retT
  where
    getExprAndUpdateCtx c l r = (tok CurlyClose $> []) <|> (expression c l r >>= \case
      x@(Variable metadata _) -> (x:) <$> getExprAndUpdateCtx c (metadata : l) r
      x -> (x:) <$> getExprAndUpdateCtx c l r)

validArgNumber :: Int -> String -> [(Type, String)] -> Parser [(Type, String)]
validArgNumber _ _ args@[_, _] = pure args
validArgNumber start name args = getOffset >>= (\offset -> failI start $ errOpArgs offset (length args) name) . subtract (start - 1)
