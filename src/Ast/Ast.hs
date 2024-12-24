{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use isNothing" #-}

module Ast.Ast
  (
    -- main
    Ast(..),
    tokenToAst,

    -- test
    runParser,
    function
  ) where

import Debug.Trace (trace)

import Text.Megaparsec (Parsec, single, eof, satisfy, choice, runParser, MonadParsec(..), setOffset, getOffset, optional, someTill)
import Data.Void (Void)
import Data.Functor (($>))
import Data.List (find)
import Control.Applicative ((<|>))
import Control.Monad (void)

import Parser.Token (MyToken(..), Identifier(..), Literal(..), Type(..))
import Ast.Error

type Parser = Parsec Void [MyToken]

data SubExpression
  = VariableCall { varName :: String }
  | FunctionCall { fnName :: String, fnArgs :: [SubExpression]}
  | OperatorCall { opName :: String, opArgs :: [SubExpression]}
  | Literal Literal
  | Builtin { builtinName :: String } -- operator + - * / -- this should only be set my default at the start of the parsing and handled by the vm
  deriving (Show, Eq)

data Expression
  = SubExpression SubExpression
  | Variable { varMeta :: (Type, String), fnValue :: SubExpression } -- variable creation inside a function
  | Return { retValue :: SubExpression }
  | IfThenElse { ifCond :: SubExpression, thenExpr :: Expression, elseExpr :: Expression }
  deriving (Show, Eq)

data Ast
  = Structure { structName :: String, structMember :: [(Type, String)] }
  | Function { fnName :: String, fnArgs :: [(Type, String)], fnRetType :: Type, fnBody :: [Expression] }
  | Operator { opName :: String, opPrecedence :: Int, opRetType :: Type, opArgLeft :: (Type, String), opArgRight :: (Type, String), opBody :: [Expression] }
  deriving (Show, Eq)

type Ctx = [Ast]

builtin :: [Ast]
builtin =
    [ Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
    , Operator {opName = "*", opPrecedence = 7, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "*"]}
    , Function {fnName = "print", fnArgs = [], fnRetType = VoidType, fnBody = [SubExpression $ Builtin "print"]} -- todo: im not really sure about this one (maybe we need the any type)
    ]

tokenToAst :: Parser Ctx
tokenToAst = ast builtin

ast :: Ctx -> Parser Ctx
ast ctx = (eof $> []) <|> ((function ctx <|> operator ctx <|> structure <|> failN errTopLevelDef) >>= (\x -> ast (x : ctx)))

tok :: MyToken -> Parser MyToken
tok = single

sym :: Parser String
sym = satisfy isSym >>= (\t -> case t of
  Identifier (SymbolId name) -> pure name
  _ -> failN errImpossibleCase
  )
  where
    isSym (Identifier (SymbolId _)) = True
    isSym _ = False

types :: Bool -> Parser Type
types canBeVoid = choice (t ++ vt) <|> failN (errExpectedType canBeVoid)
  where
    t =
      [ tok (Type CharType) $> CharType
      , tok (Type BoolType) $> BoolType
      , tok (Type IntType) $> IntType
      , tok (Type FloatType) $> FloatType
      , tok (Type StrType) $> StrType
      , tok (Type ArrType) $> ArrType
      ]
    vt
      | canBeVoid = [tok (Type VoidType) $> VoidType]
      | otherwise = []

failN :: (MonadParsec e s m, MonadFail m) => String -> m a
failN err = (setOffset . (+ 1) =<< getOffset) *> fail err

-- The function exists to be able to return the value expected
notTaken :: [String] -> (a -> String) -> a -> Parser a
notTaken names f x
  | name `elem` names = fail $ errNameTaken name
  | otherwise = pure x
  where name = f x

getFnName :: [String] -> Parser String
getFnName names = tok FunctionKw *> sym >>= notTaken names id

getFnArgs :: [String] -> Parser [(Type, String)]
getFnArgs names = tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs names)
  where
    getAllArgs names' = do
      arg <- (,) <$> types False <*> (sym >>= notTaken names' id)
      endFound <- tok ParenClose <|> tok Comma
      case endFound of
        ParenClose -> pure [arg]
        Comma -> (arg :) <$> getAllArgs (snd arg : names')
        _ -> failN errImpossibleCase

getFnRetType :: Parser Type
getFnRetType = tok Arrow *> types True

getNames :: Ctx -> [String]
getNames [] = []
getNames (Structure {structName = name}:rst) = name : getNames rst
getNames (Function {fnName = name}:rst) = name : getNames rst
getNames (Operator {opName = name}:rst) = name : getNames rst

type LocalVariable = [(Type, String)]
type RetType = Type

expression :: Ctx -> LocalVariable -> RetType -> Parser Expression
expression ctx locVar retT =
      exprReturn ctx locVar retT
  <|> exprIf ctx locVar retT
  <|> exprVariable ctx locVar retT
  <|> exprSubexpr ctx locVar retT

exprIf :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprIf _ _ _ = failN $ errTodo "if expression"

exprReturn :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprReturn _ _ VoidType = failN errVoidRet
exprReturn ctx locVar retT = do
  void (tok ReturnKw)
  subexpr <- subexpression ctx locVar
  case subexpr of
    (VariableCall x) -> let v = find (\(_, n) -> n == x) locVar in
      case v of
        Just (t, _)
          | t == retT -> return $ Return subexpr
          | otherwise -> failN $ errRetType (show retT) (show t)
        Nothing -> failN errImpossibleCase
    _ -> failN $ errTodo "return (u did only variable call)"

exprVariable :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprVariable _ _ _ = failN $ errTodo "variable creation expression"

exprSubexpr :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprSubexpr _ _ _ = failN $ errTodo "sub expression"

data Group = GGr [Group] | GLit Parser.Token.Literal | GFn String [Group] | GVar String | GOp String [Group] deriving (Show) -- should contain an index to make better errors

getGroup :: Parser Group
getGroup = choice
  [ eof *> fail errEndSubexpr
  , GGr <$> (tok ParenOpen *> someTill getGroup (tok ParenClose) <|> failN errEmptyGroup)
  , GLit <$> glit
  , GVar <$> try gsym <* notFollowedBy (tok ParenOpen)
  , GFn <$> gsym <*> (tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs))
  , GOp <$> gop <*> pure []
  ]
  where
    glit = satisfy (\case
      Parser.Token.Literal _ -> True
      _ -> False) >>= (\case
          (Parser.Token.Literal x) -> pure x
          _ -> failN errImpossibleCase)
    gsym = satisfy (\case
      Parser.Token.Identifier (SymbolId _) -> True
      _ -> False) >>= (\case
          (Parser.Token.Identifier (SymbolId x)) -> pure x
          _ -> failN errImpossibleCase)
    gop = satisfy (\case
      Parser.Token.Identifier (OperatorId _) -> True
      _ -> False) >>= (\case
          (Parser.Token.Identifier (OperatorId x)) -> pure x
          _ -> failN errImpossibleCase)
    getAllArgs = do
      arg <- getGroup
      endFound <- tok ParenClose <|> tok Comma
      case endFound of
        ParenClose -> pure [arg]
        Comma -> (arg :) <$> getAllArgs
        _ -> failN errImpossibleCase

type Idx = Int
type Prec = Int
type Name = String

mountGroup :: Ctx -> [Group] -> Parser Group
mountGroup _ [] = fail errEmptyExpr -- should never happen
mountGroup _ [x@(GLit {})] = pure x
mountGroup _ [x@(GFn {})] = pure x
mountGroup _ [x@(GVar {})] = pure x
mountGroup _ [x@(GOp _ [_, _])] = pure x
mountGroup ctx (GGr group: rst) = mountGroup ctx group >>= mountGroup ctx . (:rst)
mountGroup ctx group = do
  trace ("group=" ++ show group) (pure ())
  opId <- calcPrec Nothing (-1) 1 group
  trace ("opid=" ++ show opId) (pure ())
  case opId of
    Nothing -> fail errImpossibleCase
    (Just (x, name))
      | x <= 0 -> fail $ errMissingOperand "left" name
      | x >= length group - 1 -> fail $ errMissingOperand "right" name
      | otherwise -> let l = group !! (x - 1)
                         r = group !! (x + 1) in
                        case l of
                          (GOp _ []) -> fail $ errMissingOperand "left" name
                          _ -> case r of
                            (GOp _ []) -> fail $ errMissingOperand "right" name
                            _ -> mountGroup ctx (take (x - 1) group ++ (GOp (getOpName (group !! x)) [l, r] : drop (x + 2) group))
  where
    calcPrec :: Maybe (Prec, Name) -> Idx -> Idx -> [Group] -> Parser (Maybe (Idx, Name))
    calcPrec Nothing _ _ [] = pure Nothing
    calcPrec (Just (_, name)) idx _ [] = pure $ Just (idx, name)
    calcPrec prec idx idx2 (GOp name []: rst) = case find (isSame name) ctx of
      Nothing -> fail $ errOpNotDefined name
      Just (Operator {..}) -> case prec of
        Nothing -> calcPrec (Just (opPrecedence, opName)) (idx + idx2) 1 rst
        Just (oldPrec, _)
          | oldPrec < opPrecedence -> calcPrec (Just (opPrecedence, opName)) (idx + idx2) 1 rst
          | otherwise -> calcPrec prec idx (idx2 + 1) rst
      Just _ -> fail errImpossibleCase
    calcPrec prec idx idx2 (_: rst) = calcPrec prec idx (idx2 + 1) rst

    isSame name (Operator {..}) = name == opName
    isSame _ _ = False

getOpName :: Group -> String
getOpName (GOp name _) = name
getOpName _ = error "error: the token isn't an operator"

validateSubexpr :: Ctx -> LocalVariable -> SubExpression -> Parser ()
validateSubexpr ctx locVar subex = failN $ errTodo "validate subexpr"

toSubexpr :: Group -> Parser SubExpression
toSubexpr group = failN $ errTodo "validate to subexpr"

subexpression :: Ctx -> LocalVariable -> Parser SubExpression
subexpression ctx locVar = do
  group <- someTill getGroup (tok SemiColon) <|> failN errEmptyExpr
  mount <- mountGroup ctx group
  subex <- trace (show mount) (toSubexpr mount)
  validateSubexpr ctx locVar subex
  return subex

getFnBody :: Ctx -> LocalVariable -> RetType -> Parser [Expression]
getFnBody ctx locVar retT = do
  void (tok CurlyOpen) <|> failN errExpectedStartBody
  expr <- getExprAndUpdateCtx ctx locVar retT
  void (tok CurlyClose) <|> failN errExpectedEndBody
  return expr
  where
    getExprAndUpdateCtx :: Ctx -> LocalVariable -> RetType -> Parser [Expression]
    getExprAndUpdateCtx c l r = do
      next <- optional $ lookAhead $ tok CurlyClose
      case next of
        Just CurlyClose -> pure []
        _ -> do
          expr <- expression c l r
          case expr of
            x@(Variable metadata _) -> (:) x <$> getExprAndUpdateCtx c (metadata : l) r
            x@(Return {}) -> pure [x]
            x@(SubExpression {}) -> (:) x <$> getExprAndUpdateCtx c l r
            x@(IfThenElse {}) -> (:) x <$> getExprAndUpdateCtx c l r


function :: Ctx -> Parser Ast
function ctx = do
  name <- getFnName (getNames ctx)
  args <- getFnArgs (name : getNames ctx)
  retT <- getFnRetType
  let shellFn = Function name args retT []
  body <- getFnBody (shellFn : ctx) args retT
  return $ Function name args retT body

operator :: Ctx -> Parser Ast
operator _ = tok (Type CharType) $> Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
-- temporary

structure :: Parser Ast
structure = tok (Type CharType) $> Operator {opName = "+", opPrecedence = 6, opRetType = IntType, opArgLeft = (IntType, "l"), opArgRight = (IntType, "r"), opBody = [SubExpression $ Builtin "+"]}
-- tu peux delete ce qui est au dessus c juste pour pouvoir compiler
