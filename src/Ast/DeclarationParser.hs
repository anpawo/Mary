{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopLevelDeclaration
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ast.DeclarationParser (structure, function, operator, constraint, atom) where

import Parser.Token
import Ast.Ast
import Ast.ExpressionParser
import Ast.TokenParser
import Data.Functor (($>))
import Control.Applicative ((<|>), optional)
import Utils.Lib
import Ast.Error
import Text.Megaparsec (getOffset, satisfy, MonadParsec (try))
import Data.Maybe (fromJust)

getMembers :: Ctx -> [String] -> Parser [(String, Type)]
getMembers ctx names = (tok CurlyOpen <|> failN errExpectedStartField) *> (tok CurlyClose $> [] <|> fields names)
  where
    fields n = (,)
      <$> ((textIdentifier >>= notTaken n) <|> failN errExpectedFieldName)
      <*> ((tok Colon *> types ctx False True) <|> failN errExpectedColon)
        >>= \a -> (tok CurlyClose $> [a]) <|> (tok Comma *> ((a :) <$> fields (fst a : n))) <|> failN errExpectedCommaOrCurly

atom :: Ctx -> Parser Ast
atom ctx = (`Structure` []) <$> (tok AtomKw *> textIdentifier >>= notTaken (getNames ctx))

structure :: Ctx -> Parser Ast
structure ctx = do
  name <- structType >>= notTaken (getNames ctx) . stTyName
  let shellStruct = Structure name []
  members <- getMembers (shellStruct : ctx) (name : getNames ctx)
  return $ Structure name members

getFnArgs :: Ctx -> [String] -> Parser [(Type, String)]
getFnArgs ctx names = (tok ParenOpen <|> failN errExpectedArgs) *> (tok ParenClose $> [] <|> args names)
  where
    args n = (\n' t -> (t, n'))
      <$> ((textIdentifier <|> failN errExpectedArgName) >>= notTaken n)
      <*> ((tok Colon <|> failN errExpectedColon) *> types ctx False True) >>=
        \a -> (tok ParenClose $> [a]) <|> (tok Comma *> ((a :) <$> args (snd a : n))) <|> failN errExpectedCommaOrParen

function :: Ctx -> Parser Ast
function ctx = do
  isBuiltin <- tok FunctionKw *> optional (tok BuiltinKw)
  name <- (textIdentifier <|> failN errMisingNameFn) >>= notTaken (getNames ctx)
  args <- getFnArgs ctx (name : getNames ctx)
  retT <- (tok Arrow <|> failN errMissingRetT) *> ((satisfy isStructLiteral *> fail errMisingBody) <|> types ctx True True)
  case isBuiltin of
    Just _ -> pure $ Function name args retT []
    Nothing -> Function name args retT <$> getFnBody (Function name args retT [] : ctx) args retT

operator :: Ctx -> Parser Ast
operator ctx = do
  isBuiltin <- tok OperatorKw *> optional (tok BuiltinKw)
  name <- (operatorIdentifier <|> (try textIdentifier >>= \s -> if s == "is" then pure s else fail errMisingNameOp) <|> failN errMisingNameOp) >>= notTaken (getNames ctx)
  prcd <- tok PrecedenceKw *> (intLiteral <|> failN errMisingPrecOp) <|> pure 0
  args <- getOffset >>= (\offset -> getFnArgs ctx (name : getNames ctx) >>= validArgNumber offset name) . (+1)
  retT <- (tok Arrow <|> failN errMissingRetT) *> ((satisfy isStructLiteral *> fail errMisingBody) <|> types ctx True True)
  case isBuiltin of
    Just _ -> pure $ Operator name prcd retT (head args) (args !! 1) []
    Nothing -> Operator name prcd retT (head args) (args !! 1) <$> getFnBody (Operator name prcd retT (head args) (args !! 1) [] : ctx) args retT

constraint :: Ctx -> Parser Ast
constraint ctx = Constraint <$> (constraintType >>= notTaken (getNames ctx) . fromJust . crTyName) <*> ((tok Assign *> constrTypes) <|> failN errMisingEqual)
  where
    constrTypes = types ctx False False >>= \t -> (tok Pipe *> ((t:) <$> constrTypes)) <|> pure [t]

branchEnd :: Type -> [Expression] -> Parser [Expression]
branchEnd VoidType x = pure x
branchEnd retTy [] = fail $ errMissingRet $ show retTy
branchEnd _ x@[Return {}] = pure x
branchEnd retTy x@[IfThenElse {..}] = branchEnd retTy thenExpr >> branchEnd retTy elseExpr >> pure x
branchEnd retTy (x: xs) = (x:) <$> branchEnd retTy xs

getFnBody :: Ctx -> LocalVariable -> Type -> Parser [Expression]
getFnBody ctx locVar retT = (tok CurlyOpen <|> failN errStartBody) *> getExprAndUpdateCtx ctx locVar retT >>= branchEnd retT
  where
    getExprAndUpdateCtx c l r = (tok CurlyClose $> []) <|> (expression c l r >>= \case
      x@(Variable metadata _) -> (x:) <$> getExprAndUpdateCtx c (metadata : l) r
      x -> (x:) <$> getExprAndUpdateCtx c l r)

validArgNumber :: Int -> String -> [(Type, String)] -> Parser [(Type, String)]
validArgNumber _ _ args@[_, _] = pure args
validArgNumber start name args = getOffset >>= (\offset -> failI start $ errOpArgs offset (length args) name) . subtract (start - 1)
