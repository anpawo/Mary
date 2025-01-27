{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module contains functions for building and manipulating abstract syntax trees.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ast.TreeBuilder (
  -- Types
  Group(..),
  -- Functions
  validLit,
  getGroup,
  mountGroup,
  calcPrec,
  getOpName,
  getGrIdx,
  getGrType,
  validateMount,
  toSubexpr,
  fixOp,
  subexpression
) where

import Ast.Ast
import Parser.Token (Type(..), Literal (..), MyToken (..))
import Text.Megaparsec (getOffset, eof, choice, someTill, satisfy, notFollowedBy, someTill_, try)
import Ast.Error
import Utils.Lib
import Data.Foldable (find, traverse_)
import Ast.TokenParser
import Data.Functor (($>))
import Control.Applicative ((<|>), Alternative (empty, some))
import Data.List (singleton)
import Data.Maybe (fromJust, isNothing)
import Control.Monad (when, unless)
import Text.Printf (printf)
import Data.Tuple (swap)

data Group
  = GGr { getIndex :: Int, ggValue :: [Group]}
  | GLit { getIndex :: Int, glValue :: Literal}
  | GFn { getIndex :: Int, gfnName :: String, gfnArgs :: [Group]}
  | GVar { getIndex :: Int, gvarName :: String}
  | GOp { getIndex :: Int, gopName :: String, gopArgs :: [Group]}
  deriving (Show)

validLit :: Int -> Ctx -> LocalVariable -> Literal -> Parser Literal
validLit o ctx locVar (ListLitPre toks) = validLit o ctx locVar $ foldr (\toks' acc -> StructLitPre "elem" [("data", toks'), ("next", [Literal acc])]) (StructLit "empty" []) toks
validLit o ctx locVar (StructLitPre name toks) = validLit o ctx locVar . StructLit name =<< mapM tosub toks
  where
    tosub :: (String, [MyToken]) -> Parser (String, SubExpression)
    tosub (n, v) = case (,) n <$> run (subexpression ctx locVar eof) v of
      Left err -> fail $ printf ";%s%s" (show o) $ prettyPrintError "struct error" [] v err
      Right suc -> pure suc
validLit o ctx locVar (ArrLitPre t toks) = validLit o ctx locVar . ArrLit t =<< mapM tosub toks
  where
    tosub :: [MyToken] -> Parser SubExpression
    tosub v = case run (subexpression ctx locVar eof) v of
      Left err -> fail $ printf ";%s%s" (show o) $ prettyPrintError "array error" [] v err
      Right suc -> pure suc
validLit _ ctx locVar st@(StructLit name subexpr) =  case find (\a -> isStruct a && getName a == name) ctx of
  Just (Structure _ kv) -> mapM (\(n, v) -> (,) n <$> getType ctx locVar v) subexpr >>= \case
    kv'
      | kv == kv' -> pure st
      | otherwise -> fail $ errInvalidStructure name kv
  _ -> fail $ errStructureNotBound name
validLit _ ctx locVar arr@(ArrLit t subexp) = mapM (getType ctx locVar) subexp >>= (\ok -> if ok then pure arr else fail $ errInvalidArray (show t)) . all (== t)
validLit _ _ _ lit = pure lit


parseLambdaArgs :: Ctx -> [String] -> Parser [(Type, String)]
parseLambdaArgs ctx names = (,) <$> (textIdentifier >>= notTaken names) <*> parseArgType >>=
  \a -> tok Arrow $> [swap a] <|> (swap a:) <$> parseLambdaArgs ctx (fst a : names)
  where parseArgType = tok Colon *> types ctx False True <|> pure AnyType

parseLambdaBody :: Ctx -> LocalVariable -> Parser SubExpression
parseLambdaBody ctx vars = some (getGroup ctx vars) >>= fixOp >>= mountGroup ctx >>= \m -> validateMount ctx vars m *> toSubexpr ctx m

getCapturedVariable :: LocalVariable -> SubExpression -> [String]
getCapturedVariable vars (VariableCall n)
  | any ((== n) . snd) vars = [n]
  | otherwise = []
getCapturedVariable vars (FunctionCall n args)
  | any ((== n) . snd) vars = n : concatMap (getCapturedVariable vars) args
  | otherwise = concatMap (getCapturedVariable vars) args
getCapturedVariable vars (Lit (ArrLit _ xs)) = concatMap (getCapturedVariable vars) xs
getCapturedVariable vars (Lit (StructLit _ xs)) = concatMap (getCapturedVariable vars . snd) xs
getCapturedVariable vars (Lit (LambdaLit {lambdaBody = x})) = getCapturedVariable vars x
getCapturedVariable _ (Lit _) = []

parseLambda :: Ctx -> LocalVariable -> Parser MyToken
parseLambda ctx vars = tok BackSlash *> do
  args <- (tok Arrow $> []) <|> parseLambdaArgs ctx (map snd vars ++ getNames ctx)
  body <- parseLambdaBody ctx (vars ++ args)
  retTy <- getType ctx (vars ++ args) body
  let captured = getCapturedVariable vars body
  return $ Literal $ LambdaLit captured args body retTy


getGroup :: Ctx -> LocalVariable -> Parser Group
getGroup ctx locVar = getOffset >>= (\o -> choice
    [ eof *> fail errEndSubexpr
    , try $ GOp o <$> (textIdentifier >>= \s -> if s == "is" then pure s else empty) <*> pure []
    , GGr o <$> (tok ParenOpen *> (someTill (getGroup ctx locVar <|> failN errEndParen) (tok ParenClose) <|> failN errEmptyParen))
    , GLit o <$> (validLit o ctx locVar . literal =<< (satisfy isLiteral <|> parseLambda ctx locVar))
    , try $ GLit o <$> (textIdentifier >>= \atom -> case find (\a -> isStruct a && getName a == atom) ctx of
      Just (Structure _ []) -> pure $ StructLit atom []
      _ -> empty)
    , try $ GVar o <$> textIdentifier <* notFollowedBy (tok ParenOpen)
    , GFn o <$> textIdentifier <*> (tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs o <|> failN errEndParen))
    , GOp o <$> operatorIdentifier <*> pure []
    ]) . (+1)
  where
    getAllArgs offset = do
      (arg, endFound) <- someTill_ (getGroup ctx locVar) ((tok ParenClose $> True) <|> (tok Comma $> False))
      if endFound
        then pure [GGr offset arg]
        else (GGr offset arg :) <$> getAllArgs offset

mountGroup :: Ctx -> [Group] -> Parser Group
mountGroup _ [] = fail errEmptyExpr
mountGroup _ [x@(GLit {})] = pure x
mountGroup ctx [GFn index name args] = GFn index name <$> mapM (mountGroup ctx . singleton) args
mountGroup _ [x@(GVar {})] = pure x
mountGroup _ [x@(GOp _ _ [_, _])] = pure x
mountGroup ctx [GGr idx [GOp idx' n []]] = case find (\a -> isOp a && getName a == n) ctx of
  (Just (Operator {..})) -> pure $ GLit idx $ ClosureLit opName [fst opArgLeft, fst opArgRight] opRetType
  _ -> failI idx' $ errOperatorNotBound n
mountGroup ctx [GGr idx [x@(GVar _ n)]] = case find (\a -> isFn a && getName a == n) ctx of
  (Just (Function {..})) -> pure $ GLit idx $ ClosureLit fnName (map fst fnArgs) fnRetType
  _ -> pure x
mountGroup ctx [GGr _ group] = mountGroup ctx group
mountGroup ctx group = calcPrec ctx Nothing (-1) 1 group >>= \case
    Nothing -> let errIdx = getGrIdx (head group) in (getOffset >>= (failI errIdx . errInvalidExpr) . subtract errIdx)
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
getGrType ctx locVar (GFn index name _) expected = case find ((== name) . snd) locVar of
  Just (FunctionType _ t, _) -> when (t /= expected) (failI index $ errInvalidFnType name (show expected) (show t))
  _ -> case find (\a -> isFn a && getName a == name) ctx of
    Just (Function _ _ t _) -> when (t /= expected) (failI index $ errInvalidFnType name (show expected) (show t))
    _ -> failI index $ errFunctionNotBound name
getGrType _ locVar (GVar index name) expected = case find ((== name) . snd) locVar of
  Nothing -> failI index $ errVariableNotBound name
  Just a -> let t = fst a in when (t /= expected) (failI index $ errInvalidVarType name (show expected) (show t))
getGrType _ _ (GLit index lit) expected = unless (isType expected lit) $ failI index $ errInvalidLitType (show expected) (show $ getLitType lit)
getGrType ctx locVar g@(GGr _ _) expected = mountGroup ctx [g] >>= \m -> getGrType ctx locVar m expected

validateMount :: Ctx -> LocalVariable -> Group -> Parser ()
validateMount ctx locVar g@(GGr _ _) = mountGroup ctx [g] >>= \m -> validateMount ctx locVar m
validateMount _ locVar (GVar index name) = when (isNothing $ find ((== name) . snd) locVar) (failI index $ errVariableNotBound name)
validateMount _ _ (GLit {}) = pure ()
validateMount ctx locVar (GFn index name args) = case find (\a -> isFn a && getName a == name) ctx of
  Just (Function _ args' _ _)
    | expected /= found -> failI index $ errInvalidNumberOfArgument name expected found
    | otherwise -> mapM_ (\((t, _), g) -> getGrType ctx locVar g t) (zip args' args) >> traverse_ (validateMount ctx locVar) args
    where
      expected = length args'
      found = length args
  _ -> case find ((== name) . snd) locVar of
    Just (FunctionType args' _, _)
      | expected /= found -> failI index $ errInvalidNumberOfArgument name expected found
      | otherwise -> mapM_ (\(t, g) -> getGrType ctx locVar g t) (zip args' args) >> traverse_ (validateMount ctx locVar) args
      where
        expected = length args'
        found = length args
    _ -> failI index $ errFunctionNotBound name
validateMount ctx locVar (GOp index name [l, r]) = case find (\a -> isOp a && getName a == name) ctx of
  Just (Operator _ _ _ (tl, _) (tr, _) _) -> getGrType ctx locVar l tl >> getGrType ctx locVar r tr >> validateMount ctx locVar l >> validateMount ctx locVar r
  _ -> failI index $ errOperatorNotBound name
validateMount _ _ (GOp index _ _) = failI index $ errImpossibleCase "validateMount GOp"

toSubexpr :: Ctx -> Group -> Parser SubExpression
toSubexpr ctx (GGr _ group) = mountGroup ctx group >>= \m -> toSubexpr ctx m
toSubexpr _ (GLit _ lit) = pure $ Lit lit
toSubexpr ctx (GFn _ name body) = FunctionCall name <$> traverse (toSubexpr ctx) body
toSubexpr _ (GVar _ name) = pure $ VariableCall name
toSubexpr ctx (GOp _ name body) = FunctionCall name <$> traverse (toSubexpr ctx) body

fixOp :: [Group] -> Parser [Group]
fixOp [] = pure []
fixOp (dot@(GOp _ "." []): GVar idx name:xs) = (dot:) . (GLit idx (StringLit name):) <$> fixOp xs
fixOp (GOp index "." []: _:_) = failI (index + 1) errExpectedField
fixOp (x@(GVar {}):xs) = (x:) <$> fixOp xs
fixOp (x@(GOp {}):xs) = (x:) <$> fixOp xs
fixOp ((GGr idx gr):xs) = ((:) . GGr idx <$> fixOp gr) <*> fixOp xs
fixOp ((GFn idx name gr):xs) = ((:) . GFn idx name <$> fixOp gr) <*> fixOp xs
fixOp (x@(GLit {}):xs) = (x:) <$> fixOp xs

subexpression :: Ctx -> LocalVariable -> Parser a -> Parser SubExpression
subexpression ctx locVar endSubexpr = someTill (getGroup ctx locVar <|> failN errEndExpr) endSubexpr <|> failN errEmptyExpr >>= fixOp >>=
  mountGroup ctx >>= \m -> validateMount ctx locVar m *> toSubexpr ctx m
