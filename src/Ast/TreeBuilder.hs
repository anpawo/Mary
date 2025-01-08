{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TreeBuilder
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ast.TreeBuilder (subexpression) where

import Ast.Ast
import Parser.Token (Type, Literal (..), MyToken (..))
import Text.Megaparsec (getOffset, eof, choice, someTill, satisfy, notFollowedBy, someTill_, try)
import Ast.Error
import Utils.Lib
import Data.Foldable (find, traverse_)
import Ast.TokenParser
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.List (singleton)
import Data.Maybe (fromJust, isNothing)
import Control.Monad (when, unless)

data Group
  = GGr { getIndex :: Int, ggValue :: [Group]}
  | GLit { getIndex :: Int, glValue :: Literal}
  | GFn { getIndex :: Int, gfnName :: String, gfnArgs :: [Group]}
  | GVar { getIndex :: Int, gvarName :: String}
  | GOp { getIndex :: Int, gopName :: String, gopArgs :: [Group]}
  deriving (Show)

validLit :: Ctx -> LocalVariable -> Literal -> Parser Literal
validLit ctx locVar (StructLitPre name toks) = validLit ctx locVar . StructLit name =<< mapM tosub toks
  where
    tosub (n, v) = case (,) n <$> run (subexpression ctx locVar eof) v of
      Left err -> fail $ ";" ++ prettyPrintError v err
      Right suc -> pure suc
validLit ctx locVar (ArrLitPre t toks) = validLit ctx locVar . ArrLit t =<< mapM tosub toks
  where
    tosub v = case run (subexpression ctx locVar eof) v of
      Left err -> fail $ ";" ++ prettyPrintError v err
      Right suc -> pure suc
validLit ctx locVar st@(StructLit name subexpr) =  case find (\a -> isStruct a && getName a == name) ctx of
  Just (Structure _ kv) -> mapM (\(n, v) -> (,) n <$> getType ctx locVar v) subexpr >>= \case
    kv'
      | kv == kv' -> pure st
      | otherwise -> fail $ errInvalidStructure name kv
  _ -> failN $ errStructureNotBound name
validLit ctx locVar arr@(ArrLit t subexp) = any (/= t) <$> mapM (getType ctx locVar) subexp $> arr
validLit _ _ lit = pure lit


getGroup :: Ctx -> LocalVariable -> Parser Group
getGroup ctx locVar = getOffset >>= (\offset -> choice
    [ eof *> fail errEndSubexpr
    , GGr offset <$> (tok ParenOpen *> (someTill (getGroup ctx locVar) (tok ParenClose) <|> fail errEmptyParen))
    , GLit offset <$> (validLit ctx locVar . literal =<< satisfy isLiteral)
    , try $ GVar offset <$> textIdentifier <* notFollowedBy (tok ParenOpen)
    , GFn offset <$> textIdentifier <*> (tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs offset))
    , GOp offset <$> operatorIdentifier <*> pure []
    ]) . (+1)
  where
    getAllArgs offset = do
      (arg, endFound) <- someTill_ (getGroup ctx locVar) ((tok ParenClose $> True) <|> (tok Comma $> False))
      if endFound
        then pure [GGr offset arg]
        else (GGr offset arg :) <$> getAllArgs offset
        -- (tok ParenClose $> [arg]) <|> (tok Comma *> ((arg :) <$> getAllArgs))
      -- (args, sep) -> (tok ParenClose $> [arg]) <|> (tok Comma *> ((arg :) <$> getAllArgs))

mountGroup :: Ctx -> [Group] -> Parser Group
mountGroup _ [] = fail errEmptyExpr
mountGroup _ [x@(GLit {})] = pure x
mountGroup ctx [GFn index name args] = GFn index name <$> mapM (mountGroup ctx . singleton) args
mountGroup _ [x@(GVar {})] = pure x
mountGroup _ [x@(GOp _ _ [_, _])] = pure x
mountGroup ctx (GGr _ group: rst) = mountGroup ctx group >>= mountGroup ctx . (:rst)
mountGroup ctx group = calcPrec ctx Nothing (-1) 1 group >>= \case
    Nothing -> let errIdx = getGrIdx (head group) in (getOffset >>= (failI errIdx . errTooManyExpr) . subtract errIdx)
    (Just (x, name))
      | x <= 0 -> failI (getGrIdx (head group)) $ errMissingOperand "left" name
      | x >= length group - 1 -> failI (getGrIdx (last group)) $ errMissingOperand "right" name
      | otherwise ->
          let tmpL = group !! (x - 1)
              tmpR = group !! (x + 1) in do
          l <- case tmpL of
            (GGr {}) -> mountGroup ctx [tmpL]
            _ -> pure tmpL
          r <- case tmpR of
            (GGr {}) -> mountGroup ctx [tmpR]
            _ -> pure tmpR
          case l of
            (GOp _ _ []) -> failI (getGrIdx (group !! x)) $ errMissingOperand "left" name
            _ -> case r of
              (GOp _ _ []) -> failI (getGrIdx (group !! x)) $ errMissingOperand "right" name
              _ -> mountGroup ctx (take (x - 1) group ++ (GOp (getGrIdx (group !! x)) (getOpName (group !! x)) [l, r] : drop (x + 2) group))

calcPrec :: Ctx -> Maybe (Int, String) -> Int -> Int -> [Group] -> Parser (Maybe (Int, String))
calcPrec _ Nothing _ _ [] = pure Nothing
calcPrec _ (Just (_, name)) idx _ [] = pure $ Just (idx, name)
calcPrec ctx prec idx idx2 (GOp tokIdx name []: rst) = case find (\a -> isOp a && getName a == name) ctx of
  Just (Operator {..}) -> case prec of
    Nothing -> calcPrec ctx (Just (opPrecedence, opName)) (idx + idx2) 1 rst
    Just (oldPrec, _)
      | oldPrec < opPrecedence -> calcPrec ctx (Just (opPrecedence, opName)) (idx + idx2) 1 rst
      | otherwise -> calcPrec ctx prec idx (idx2 + 1) rst
  _ -> failI tokIdx $ errOpNotDefined name
calcPrec ctx prec idx idx2 (_: rst) = calcPrec ctx prec idx (idx2 + 1) rst

getOpName :: Group -> String
getOpName (GOp _ name _) = name
getOpName _ = error "error: the token isn't an operator"

getGrIdx :: Group -> Int
getGrIdx (GOp index _ _) = index
getGrIdx (GGr index _) = index
getGrIdx (GLit index _) = index
getGrIdx (GFn index _ _) = index
getGrIdx (GVar index _) = index

getGrType :: Ctx -> LocalVariable -> Group -> Type -> Parser ()
getGrType ctx _ (GOp index name _) expected = let t = opRetType $ fromJust $ find (\a -> isOp a && getName a == name) ctx in
  when (t /= expected) (failI index $ errInvalidOpType name (show expected) (show t))
getGrType ctx _ (GFn index name _) expected = let t = opRetType $ fromJust $ find (\a -> isFn a && getName a == name) ctx in
  when (t /= expected) (failI index $ errInvalidFnType name (show expected) (show t))
getGrType _ locVar (GVar index name) expected = case find ((== name) . snd) locVar of
  Nothing -> failI index $ errVariableNotBound name
  Just a -> let t = fst a in when (t /= expected) (failI index $ errInvalidVarType name (show expected) (show t))
getGrType _ _ (GLit index lit) expected = unless (isType expected lit) $ failI index $ errInvalidLitType (show expected) (show $ getLitType lit)
getGrType _ _ (GGr index _) _ = failI index $ errImpossibleCase "getGrType GGr"

validateMount :: Ctx -> LocalVariable -> Group -> Parser ()
validateMount _ _ (GGr index _) = failI index $ errImpossibleCase "validateMount"
validateMount _ locVar (GVar index name) = when (isNothing $ find ((== name) . snd) locVar) (failI index $ errVariableNotBound name)
validateMount _ _ (GLit {}) = pure ()
validateMount ctx locVar (GFn index name args) = case find (\a -> isFn a && getName a == name) ctx of
  Just (Function _ args' _ _)
    | expected /= found -> failI index $ errInvalidNumberOfArgument name expected found
    | otherwise -> mapM_ (\((t, _), g) -> getGrType ctx locVar g t) (zip args' args) >> traverse_ (validateMount ctx locVar) args
    where
      expected = length args'
      found = length args
  _ -> failI index $ errFunctionNotBound name
validateMount ctx locVar (GOp index name [l, r]) = case find (\a -> isOp a && getName a == name) ctx of
  Just (Operator _ _ _ (tl, _) (tr, _) _) -> getGrType ctx locVar l tl >> getGrType ctx locVar r tr >> validateMount ctx locVar l >> validateMount ctx locVar r
  _ -> failI index $ errOperatorNotBound name
validateMount _ _ (GOp index _ _) = failI index $ errImpossibleCase "validateMount GOp"

toSubexpr :: Group -> Parser SubExpression
toSubexpr (GGr _ _) = fail $ errImpossibleCase "toSubexpr GGr"
toSubexpr (GLit _ lit) = pure $ Lit lit
toSubexpr (GFn _ name body) = FunctionCall name <$> traverse toSubexpr body
toSubexpr (GVar _ name) = pure $ VariableCall name
toSubexpr (GOp _ name body) = FunctionCall name <$> traverse toSubexpr body

fixOp :: [Group] -> Parser [Group]
fixOp [] = pure []
fixOp (dot@(GOp _ "." []): GVar idx name:xs) = (dot:) . (GLit idx (StringLit name):) <$> fixOp xs
fixOp (GOp index "." []: _:_) = failI (index + 1) errExpectedField
fixOp (x:xs) = (x:) <$> fixOp xs

subexpression :: Ctx -> LocalVariable -> Parser a -> Parser SubExpression
subexpression ctx locVar endSubexpr =
  someTill (getGroup ctx locVar) endSubexpr <|> failN errEmptyExpr >>= fixOp >>= mountGroup ctx >>= \m -> validateMount ctx locVar m *> toSubexpr m
