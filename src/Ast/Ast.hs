{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module defines the abstract syntax tree (AST).
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Ast.Ast
  ( Ast(..)
  , Expression(..)
  , SubExpression(..)
  , Ctx
  , Parser
  , getNames
  , types
  , notTaken
  , LocalVariable
  , getType
  , isStruct
  , isType
  , getName
  , isOp
  , isFn
  , getCtxName
  , getLitType
) where

import Text.Megaparsec (Parsec, MonadParsec(..))

import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Functor (($>))
import Data.List (find)

import Control.Applicative ((<|>), Alternative(..))
import Parser.Token
import Ast.Error
import Ast.TokenParser
import Utils.Lib
import Control.Monad (liftM2)

type Parser = Parsec Void [MyToken]
type Ctx = [Ast]

data Expression
  = SubExpression SubExpression
  | Variable { varMeta :: (Type, String), varValue :: SubExpression }                   -- variable creation and modification inside a function
  | StructField { varName :: String, fieldName :: String, fieldValue :: SubExpression } -- modify struct field of a variable
  | Return { retValue :: SubExpression }
  | IfThenElse { ifCond :: SubExpression, thenExpr :: [Expression], elseExpr :: [Expression] }
  | While { whileCond :: SubExpression, whileExpr :: [Expression] }
  deriving (Show, Eq)

data Ast
  = Structure { structName :: String, structMember :: [(String, Type)] }
  | Constraint { constrName :: String, constrType :: [Type] }
  | Function { fnName :: String, fnArgs :: [(Type, String)], fnRetType :: Type, fnBody :: [Expression] }
  | Operator { opName :: String, opPrecedence :: Int, opRetType :: Type, opArgLeft :: (Type, String), opArgRight :: (Type, String), opBody :: [Expression] }
  deriving (Show, Eq)

type LocalVariable = [(Type, String)]

-- type check
isStruct :: Ast -> Bool
isStruct (Structure {}) = True
isStruct _              = False

isOp :: Ast -> Bool
isOp (Operator {}) = True
isOp _             = False

isFn :: Ast -> Bool
isFn (Function {}) = True
isFn _             = False

isConstr :: Ast -> Bool
isConstr (Constraint {}) = True
isConstr _               = False

-- context name
getName :: Ast -> String
getName (Structure {..}) = structName
getName (Constraint {..}) = constrName
getName (Operator {..}) = opName
getName (Function {..}) = fnName

getCtxName :: [Ast -> Bool] -> Ctx -> [String]
getCtxName _ [] = []
getCtxName fs (x:xs)
  | foldr (\f b -> b || f x) False fs = getName x : getCtxName fs xs
  | otherwise                         = getCtxName fs xs

getNames :: Ctx -> [String]
getNames = getCtxName [isStruct, isOp, isFn, isConstr]
--

types :: Ctx -> Bool -> Bool -> Parser Type
types ctx canBeVoid canBeConstraint = oneTy >>= \t -> if canBeConstraint then notFollowedBy (tok Pipe) $> t <|> constrTy t else pure t
  where
    oneTy = choicetry existingTypes <|> failN (errExpectedType canBeVoid)
    constrTy t = ConstraintType Nothing . (t:) <$> some (tok Pipe *> oneTy)

    existingTypes = [charType, boolType, nullType, intType, floatType, strType, arrayType,
      structType     >>= loadStructure . stTyName,
      constraintType >>= loadConstraint . fromJust . crTyName,
      closureType,
      textIdentifier >>= loadStructure,
      textIdentifier >>= loadConstraint
      ] ++ ([voidType | canBeVoid])

    loadConstraint name
      | name `elem` getCtxName [isConstr] ctx = pure $ ConstraintType (Just name) (loadConstraintType name)
      | otherwise                             = fail $ errConstraintNotBound name

    loadConstraintType name = constrType $ fromJust $ find (\a -> isConstr a && getName a == name) ctx

    loadStructure name
      | name `elem` getCtxName [isStruct] ctx = pure $ StructType name
      | otherwise                             = fail $ errStructureNotBound name

    closureType :: Parser Type
    closureType = liftM2 FunctionType (tok ParenOpen *> ((tok ParenClose $> []) <|> args)) (tok Arrow *> types ctx True True)
      where args = types ctx False True >>= \t -> (tok ParenClose $> [t]) <|> (tok Comma *> ((t :) <$> args))


getType :: Ctx -> LocalVariable -> SubExpression -> Parser Type
getType _ locVar (VariableCall name) = maybe (fail $ errVariableNotBound name) (pure . fst) (find ((== name) . snd) locVar)
getType ctx locVar (FunctionCall name _) = case find (\a -> (isFn a || isOp a) && getName a == name) ctx of
  Just (Function {..}) -> pure fnRetType
  Just (Operator {..}) -> pure opRetType
  _ -> case find ((== name) . snd) locVar of
    Just (FunctionType _ retType, _) -> pure retType
    _ -> fail $ errFunctionNotBound name
getType ctx locVar (Lit struct@(StructLit name lit)) = case find (\a -> isStruct a && getName a == name) ctx of
  Just (Structure _ kv) -> mapM (\(n, v) -> (,) n <$> getType ctx locVar v) lit >>= \case
    kv'
      | kv == kv' -> pure $ getLitType struct
      | otherwise -> fail $ errInvalidStructure name kv
  _ -> failN $ errStructureNotBound name
getType ctx locVar (Lit (ArrLit t lit)) = any (/= t) <$> mapM (getType ctx locVar) lit $> t
getType _ _ (Lit x) = pure $ getLitType x

isType :: Type -> Literal -> Bool
isType CharType (CharLit _) = True
isType IntType (IntLit _) = True
isType BoolType (BoolLit _) = True
isType FloatType (FloatLit _) = True
isType StrType (StringLit _) = True
isType (ArrType AnyType) (ArrLit _ _) = True
isType (ArrType t) (ArrLit t' _) = t == t'
isType (StructType n) (StructLit n' _) = n == n'
isType (ConstraintType _ t) lit = any (`isType` lit) t
isType VoidType _ = False
isType NullType NullLit = True
isType (FunctionType argsTy retTy) (ClosureLit _ argsTy' retTy') = argsTy == argsTy' && retTy == retTy'
isType (FunctionType argsTy retTy) (LambdaLit _ argsTy' _ retTy') = argsTy == map fst argsTy' && retTy == retTy'
isType AnyType (ClosureLit {}) = False
isType AnyType _ = True
isType _ _ = False -- any other combination

getLitType :: Literal -> Type
getLitType (CharLit _) = CharType
getLitType (IntLit _) = IntType
getLitType (BoolLit _) = BoolType
getLitType (FloatLit _) = FloatType
getLitType (StringLit _) = StrType
getLitType (ArrLit t _) = t
getLitType (StructLit n _) = StructType n
getLitType (StructLitPre n _) = StructType n
getLitType (ArrLitPre t _) = t
getLitType (ClosureLit _ argsTy retTy) = FunctionType argsTy retTy
getLitType (ListLitPre _) = ConstraintType Nothing [StructType "empty", StructType "elem"]
getLitType NullLit = NullType
getLitType (LambdaLit {..}) = FunctionType (map fst lambdaArgs) lambdaRetTy

notTaken :: [String] -> String -> Parser String
notTaken names name
  | name `elem` names = fail $ errNameTaken name
  | otherwise = pure name
