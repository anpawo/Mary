{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use isNothing" #-}

module Ast.Ast
  (
    -- main
    Ast(..),
    tokenToAst,
    Expression(..),
    SubExpression(..),

    -- test
    isType,
    runParser,
    function
  ) where

import Text.Megaparsec (Parsec, single, eof, satisfy, runParser, MonadParsec(..), setOffset, getOffset, optional, someTill, choice)
import Text.Printf (printf)

import Data.Void (Void)
import Data.Functor (($>))
import Data.List (find)
import Data.Foldable (traverse_)

import Control.Applicative ((<|>), Alternative(..))
import Control.Monad (void)

import Parser.Token
import Ast.Error
import Utils.Lib (choicetry, run)

type Parser = Parsec Void [MyToken]

data Expression
  = SubExpression SubExpression
  | Variable { varMeta :: (Type, String), fnValue :: SubExpression } -- variable creation and modification inside a function
  | Return { retValue :: SubExpression }
  | IfThenElse { ifCond :: SubExpression, thenExpr :: Expression, elseExpr :: Expression }
  | While { whileCond :: SubExpression, whileExpr :: [Expression] }
  deriving (Show, Eq)

data Ast
  = Structure { structName :: String, structMember :: [(String, Type)] }
  | Constraint { constrName :: String, constrType :: [Type] }
  | Function { fnName :: String, fnArgs :: [(Type, String)], fnRetType :: Type, fnBody :: [Expression] }
  | Operator { opName :: String, opPrecedence :: Int, opRetType :: Type, opArgLeft :: (Type, String), opArgRight :: (Type, String), opBody :: [Expression] }
  deriving (Show, Eq)

type Ctx = [Ast]

builtinType :: String -> Type
builtinType name = case find ((== name) . fst) ts of
  Nothing -> error $ printf "error: this function should only be called in builtins, '%s' not found " name
  Just (_, t) -> ConstraintType name t
  where
    ts = [
      ("number", [IntType, FloatType])
      ]

builtin :: Ctx
builtin =
    [
    -- types for better readability
      Constraint { constrName = "number", constrType = [IntType, FloatType] }
    -- numerical operations
    , Operator {opName = "/", opPrecedence = 7, opRetType = builtinType "number", opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "*", opPrecedence = 7, opRetType = builtinType "number", opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "-", opPrecedence = 6, opRetType = builtinType "number", opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "+", opPrecedence = 6, opRetType = builtinType "number", opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    -- comparison
    , Operator {opName = "<", opPrecedence = 5, opRetType = BoolType, opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "<=", opPrecedence = 5, opRetType = BoolType, opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = ">=", opPrecedence = 5, opRetType = BoolType, opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "==", opPrecedence = 4, opRetType = BoolType, opArgLeft = (AnyType, "l"), opArgRight = (AnyType, "r"), opBody = []}
    -- io/struct/arr functions
    ,  Operator {opName = ".", opPrecedence = 10, opRetType = AnyType, opArgLeft = (StructAnyType, "l"), opArgRight = (StrType, "r"), opBody = []} -- access structure element (for now we would do: <person>."<name>")
    , Function {fnName = "print", fnArgs = [(ArrType AnyType, "args")], fnRetType = VoidType, fnBody = []}
    , Function {fnName = "exit", fnArgs = [(IntType, "status")], fnRetType = VoidType, fnBody = []}
    , Function {fnName = "length", fnArgs = [(ArrType AnyType, "arr")], fnRetType = IntType, fnBody = []}
    , Function {fnName = "at", fnArgs = [(ArrType AnyType, "arr"), (IntType, "index")], fnRetType = AnyType, fnBody = []}
    , Function {fnName = "is", fnArgs = [(AnyType, "x"), (StrType, "type")], fnRetType = BoolType, fnBody = []}
    , Function {fnName = "getline", fnArgs = [], fnRetType = StrType, fnBody = []}
    
    -- std lib
    , Function {fnName = "not", fnArgs = [(BoolType, "cond")], fnRetType = BoolType, fnBody = []}
    , Operator {opName = ">", opPrecedence = 5, opRetType = BoolType, opArgLeft = (builtinType "number", "l"), opArgRight = (builtinType "number", "r"), opBody = []}
    , Operator {opName = "!=", opPrecedence = 4, opRetType = BoolType, opArgLeft = (AnyType, "l"), opArgRight = (AnyType, "r"), opBody = []}
    , Operator {opName = "||", opPrecedence = 4, opRetType = BoolType, opArgLeft = (BoolType, "l"), opArgRight = (BoolType, "r"), opBody = []}
    , Operator {opName = "&&", opPrecedence = 4, opRetType = BoolType, opArgLeft = (BoolType, "l"), opArgRight = (BoolType, "r"), opBody = []}
    
    -- tests
    , Structure {structName = "age", structMember = [("y", IntType), ("m", IntType), ("d", IntType)]}
    , Structure {structName = "person", structMember = [("name", StrType), ("age", StructType "age")]}
    ]

tokenToAst :: Parser Ctx
tokenToAst = ast builtin

ast :: Ctx -> Parser Ctx
ast ctx = (eof $> drop (length builtin) ctx) <|> ((function ctx <|> operator ctx <|> structure ctx <|> constraint ctx <|> failN errTopLevelDef) >>= (\x -> ast (ctx ++ [x])))

tok :: MyToken -> Parser MyToken
tok = single

sym :: Parser String
sym = satisfy isSym >>= (\case
  Identifier (SymbolId name) -> pure name
  _ -> failN $ errImpossibleCase "sym case t"
  )
  where
    isSym (Identifier (SymbolId _)) = True
    isSym _ = False

ope :: Parser String
ope = satisfy isOpe >>= (\case
  Identifier (OperatorId name) -> pure name
  _ -> failN $ errImpossibleCase "ope case t"
  )
  where
    isOpe (Identifier (OperatorId _)) = True
    isOpe _ = False

types :: Ctx -> Bool -> Parser Type -- todo: enhance it to not need the keyword struct and constraint
types ctx canBeVoid = choicetry (t ++ vt) <|> failN (errExpectedType canBeVoid)
  where
    t =
      [ tok (Type CharType) $> CharType
      , tok (Type BoolType) $> BoolType
      , tok (Type IntType) $> IntType
      , tok (Type FloatType) $> FloatType
      , tok (Type StrType) $> StrType
      , satisfy isArrayType >>= (\case
        (Type (ArrType t')) -> pure (ArrType t')
        _ -> failN $ errImpossibleCase "types arr")
      , satisfy isStructType >>= (\case
        (Type (StructType n))
          | structExists n -> pure (StructType n)
          | otherwise -> fail $ errStructureNotBound n
        _ -> failN $ errImpossibleCase "types struct")
      , satisfy isConstraintType >>= (\case
        (Type (ConstraintType n _)) -> ConstraintType n <$> getCsTypes n ctx
        _ -> failN $ errImpossibleCase "types constraint")
      , satisfy structInCtx >>= (\case
        (Identifier (SymbolId name)) -> pure (StructType name)
        _ -> failN $ errImpossibleCase "types struct")
      , satisfy constraintInCtx >>= (\case
        (Identifier (SymbolId name)) -> ConstraintType name <$> getCsTypes name ctx
        _ -> failN $ errImpossibleCase "types struct")
      ]

    vt
      | canBeVoid = [tok (Type VoidType) $> VoidType]
      | otherwise = []

    getCsTypes name [] = fail $ errConstraintNotBound name
    getCsTypes name (x: xs) = if sameName x then pure (constrType x) else getCsTypes name xs
      where
        sameName (Constraint {..}) = name == constrName
        sameName _ = False

    structExists name = any sameName ctx
      where
        sameName (Structure {..}) = name == structName
        sameName _ = False
    
    structInCtx (Identifier (SymbolId name)) = any sameName ctx
      where
        sameName (Structure {..}) = name == structName
        sameName _ = False
    structInCtx _ = False
    
    constraintInCtx (Identifier (SymbolId name)) = any sameName ctx
      where
        sameName (Constraint {..}) = name == constrName
        sameName _ = False
    constraintInCtx _ = False

    isArrayType (Type (ArrType _)) = True
    isArrayType _ = False

    isStructType (Type (StructType _)) = True
    isStructType _ = False

    isConstraintType (Type (ConstraintType {})) = True
    isConstraintType _ = False

failN :: (MonadParsec e s m, MonadFail m) => String -> m a
failN err = (setOffset . (+ 1) =<< getOffset) *> fail err

failI :: (MonadParsec e s m, MonadFail m) => Int -> String -> m a
failI idx err = setOffset idx *> fail err

-- The function exists to be able to return the value expected
notTaken :: [String] -> (a -> String) -> a -> Parser a
notTaken names f x
  | name `elem` names = fail $ errNameTaken name
  | otherwise = pure x
  where name = f x

getFnName :: [String] -> Parser String
getFnName names = tok FunctionKw *> sym >>= notTaken names id

getFnArgs :: Ctx -> [String] -> Parser [(Type, String)]
getFnArgs ctx names = tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs names)
  where
    getAllArgs names' = do
      arg <- (,) <$> types ctx False <*> (sym >>= notTaken names' id)
      endFound <- tok ParenClose <|> tok Comma
      case endFound of
        ParenClose -> pure [arg]
        Comma -> (arg :) <$> getAllArgs (snd arg : names')
        _ -> failN $ errImpossibleCase "getFnArgs case endFound"

getFnRetType :: Ctx -> Parser Type
getFnRetType ctx = tok Arrow *> types ctx True

getNames :: Ctx -> [String]
getNames [] = []
getNames (Structure {structName = name}:rst) = name : getNames rst
getNames (Function {fnName = name}:rst) = name : getNames rst
getNames (Operator {opName = name}:rst) = name : getNames rst
getNames (Constraint {constrName = name}:rst) = name : getNames rst

type LocalVariable = [(Type, String)]
type RetType = Type

expression :: Ctx -> LocalVariable -> RetType -> Parser Expression
expression ctx locVar retT = choice
  [ exprReturn ctx locVar retT
  , exprSubexpr ctx locVar retT
  , exprVariable ctx locVar retT
  , exprIf ctx locVar retT -- garice
  , exprWhile ctx locVar retT -- garice
  ]

exprIf :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprIf _ _ _ = failN $ errTodo "if expression"

exprWhile :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprWhile _ _ _ = failN $ errTodo "while expression"

exprReturn :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprReturn _ _ VoidType = failN errVoidRet
exprReturn ctx locVar retT = do
  void (tok ReturnKw)
  offset <- getOffset
  subexpr <- subexpression ctx locVar
  case subexpr of
    (VariableCall x) -> case find (\(_, n) -> n == x) locVar of
        Just (t, _)
          | t == retT -> return $ Return subexpr -- todo: check return type, todo: prevent missing return at the end
          | otherwise -> failI offset $ errRetType (show retT) (show t)
        Nothing -> failN $ errImpossibleCase "exprReturn variable call"
    (FunctionCall {fnName = name}) -> case find (\case (Operator {..}) -> opName == name;(Function {..}) -> fnName == name;_ -> False) ctx of
        Just (Operator {..})
          | opRetType == retT -> return $ Return subexpr
          | otherwise -> failI offset $ errRetType (show retT) (show opRetType)
        Just (Function {..})
          | fnRetType == retT -> return $ Return subexpr
          | otherwise -> failI offset $ errRetType (show retT) (show fnRetType)
        _ -> failN $ errImpossibleCase "exprReturn function call"
    (Lit x)
      | getLitType x == retT -> return $ Return subexpr
      | otherwise -> failI offset $ errRetType (show retT) (show $ getLitType x)

exprVariable :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprVariable _ _ _ = failN $ errTodo "variable creation expression"

exprSubexpr :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprSubexpr ctx locVar _ = SubExpression <$> subexpression ctx locVar

type Idx = Int
type Prec = Int
type Name = String

data Group
  = GGr Idx [Group]
  | GLit Idx Literal
  | GFn Idx String [Group]
  | GVar Idx String
  | GOp Idx String [Group]
  deriving (Show) -- should contain an index to make better errors

getGroup :: Ctx -> LocalVariable -> Parser Group
getGroup ctx locVar = do
  offset <- (+1) <$> getOffset
  choice
    [ eof *> fail errEndSubexpr
    , GGr offset <$> (tok ParenOpen *> (someTill (getGroup ctx locVar) (tok ParenClose) <|> fail errEmptyParen))
    , GLit offset <$> glit
    , try $ GVar offset <$> gsym <* notFollowedBy (tok ParenOpen)
    , GFn offset <$> gsym <*> (tok ParenOpen *> (tok ParenClose $> [] <|> getAllArgs))
    , GOp offset <$> gop <*> pure []
    ]
  where
    glit = satisfy (\case
      Literal _ -> True
      _ -> False) >>= (\case
          (Literal x) -> validLit ctx locVar x
          _ -> failN $ errImpossibleCase "getGroup case glit")
    gsym = satisfy (\case
      Parser.Token.Identifier (SymbolId _) -> True
      _ -> False) >>= (\case
          (Parser.Token.Identifier (SymbolId x)) -> pure x
          _ -> failN $ errImpossibleCase "getGroup case gsym")
    gop = satisfy (\case
      Parser.Token.Identifier (OperatorId _) -> True
      _ -> False) >>= (\case
          (Parser.Token.Identifier (OperatorId x)) -> pure x
          _ -> failN $ errImpossibleCase "getGroup case gop")
    getAllArgs = do
      arg <- getGroup ctx locVar
      endFound <- tok ParenClose <|> tok Comma
      case endFound of
        ParenClose -> pure [arg]
        Comma -> (arg :) <$> getAllArgs
        _ -> failN $ errImpossibleCase "getGroup case getAllArgs"

mountGroup :: Ctx -> [Group] -> Parser Group
mountGroup _ [] = fail errEmptyExpr
mountGroup _ [x@(GLit {})] = pure x
mountGroup _ [x@(GFn {})] = pure x
mountGroup _ [x@(GVar {})] = pure x
mountGroup _ [x@(GOp _ _ [_, _])] = pure x
mountGroup ctx (GGr _ group: rst) = mountGroup ctx group >>= mountGroup ctx . (:rst)
mountGroup ctx group = do
  opId <- calcPrec Nothing (-1) 1 group
  case opId of
    Nothing -> do
      let errorIndex = getGrIdx (head group)
      nbInvalidToken <- subtract errorIndex <$> getOffset
      failI errorIndex $ errTooManyExpr nbInvalidToken
    (Just (x, name))
      | x <= 0 -> failI (getOpIdx (head group)) $ errMissingOperand "left" name
      | x >= length group - 1 -> failI (getOpIdx (last group)) $ errMissingOperand "right" name
      | otherwise -> do
        let tmpL = group !! (x - 1)
        let tmpR = group !! (x + 1)
        l <- case tmpL of
          (GGr {}) -> mountGroup ctx [tmpL]
          _ -> pure tmpL
        r <- case tmpR of
          (GGr {}) -> mountGroup ctx [tmpR]
          _ -> pure tmpR
        case l of
          (GOp _ _ []) -> failI (getOpIdx (group !! x)) $ errMissingOperand "left" name
          _ -> case r of
            (GOp _ _ []) -> failI (getOpIdx (group !! x)) $ errMissingOperand "right" name
            _ -> mountGroup ctx (take (x - 1) group ++ (GOp (getOpIdx (group !! x)) (getOpName (group !! x)) [l, r] : drop (x + 2) group))
  where
    calcPrec :: Maybe (Prec, Name) -> Idx -> Idx -> [Group] -> Parser (Maybe (Idx, Name))
    calcPrec Nothing _ _ [] = pure Nothing
    calcPrec (Just (_, name)) idx _ [] = pure $ Just (idx, name)
    calcPrec prec idx idx2 (GOp tokIdx name []: rst) = case find (isSame name) ctx of
      Nothing -> failI tokIdx $ errOpNotDefined name
      Just (Operator {..}) -> case prec of
        Nothing -> calcPrec (Just (opPrecedence, opName)) (idx + idx2) 1 rst
        Just (oldPrec, _)
          | oldPrec < opPrecedence -> calcPrec (Just (opPrecedence, opName)) (idx + idx2) 1 rst
          | otherwise -> calcPrec prec idx (idx2 + 1) rst
      Just _ -> fail $ errImpossibleCase "moungGroup calcPrec case find"
    calcPrec prec idx idx2 (_: rst) = calcPrec prec idx (idx2 + 1) rst

    isSame name (Operator {..}) = name == opName
    isSame _ _ = False


getOpName :: Group -> String
getOpName (GOp _ name _) = name
getOpName _ = error "error: the token isn't an operator"

getGrIdx :: Group -> Int
getGrIdx (GOp index _ _) = index
getGrIdx (GGr index _) = index
getGrIdx (GLit index _) = index
getGrIdx (GFn index _ _) = index
getGrIdx (GVar index _) = index

getOpIdx :: Group -> Int
getOpIdx (GOp index _ _) = index
getOpIdx _ = error "error: the token isn't an operator"

validLit :: Ctx -> LocalVariable -> Literal -> Parser Literal
validLit ctx locVar (StructLitPre name toks) = validLit ctx locVar . StructLit name =<< mapM tosub toks
  where
    tosub (n, v) = case (,) n <$> run (subexpression2 ctx locVar) v of
      Left err -> fail $ ";" ++ prettyPrintError v err -- todo fix error
      Right suc -> pure suc
validLit ctx locVar (ArrLitPre t toks) = validLit ctx locVar . ArrLit t =<< mapM tosub toks
  where
    tosub v = case run (subexpression2 ctx locVar) v of
      Left err -> fail $ ";" ++ prettyPrintError v err -- todo fix error
      Right suc -> pure suc
validLit ctx locVar st@(StructLit name subexpr) =  case find (stNameIs name) ctx of
  Nothing -> failN $ errStructureNotBound name
  Just (Structure _ kv) -> do
    kv' <- mapM (\(n, v) -> (,) n <$> getType ctx locVar v) subexpr
    if kv == kv'
      then pure st
      else fail $ errInvalidStructure name kv
  Just _ -> fail $ errImpossibleCase "validLit struct"
validLit ctx locVar arr@(ArrLit t subexp) = any (/= t) <$> mapM (getType ctx locVar) subexp $> arr
validLit _ _ lit = pure lit

getType :: Ctx -> LocalVariable -> SubExpression -> Parser Type
getType _ locVar (VariableCall name) = case find ((== name) . snd) locVar of
  Just (t, _) -> pure t
  Nothing -> fail $ errVariableNotBound name
getType ctx _ (FunctionCall name _) = case find (\x -> fnNameIs name x || opNameIs name x) ctx of
  Just (Function {..}) -> pure fnRetType
  Just (Operator {..}) -> pure opRetType
  Nothing -> fail $ errFunctionNotBound name
  Just _ -> fail $ errImpossibleCase "gettype"
getType ctx locVar (Lit struct@(StructLit name lit)) = case find (stNameIs name) ctx of
  Nothing -> failN $ errStructureNotBound name
  Just (Structure _ kv) -> do
    kv' <- mapM (\(n, v) -> (,) n <$> getType ctx locVar v) lit
    if kv == kv'
      then pure $ getLitType struct
      else fail $ errInvalidStructure name kv
  Just _ -> fail $ errImpossibleCase "gettype struct"
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
isType StructAnyType (StructLit _ _) = True
isType (ConstraintType _ t) lit = any (`isType` lit) t
isType VoidType _ = False
isType AnyType _ = True
isType _ _ = False -- any other combination

getLitType :: Literal -> Type -- todo: remove ctx here becauser its not needed
getLitType (CharLit _) = CharType
getLitType (IntLit _) = IntType
getLitType (BoolLit _) = BoolType
getLitType (FloatLit _) = FloatType
getLitType (StringLit _) = StrType
getLitType (ArrLit t _) = t
getLitType (StructLit n _) = StructType n
getLitType (StructLitPre n _) = StructType n
getLitType (ArrLitPre t _) = t

getGrType :: Ctx -> LocalVariable -> Group -> Type -> Parser ()
getGrType ctx _ (GOp index name _) expected = maybe
  (error $ errImpossibleCase "getGrType GOp")
  ((\t -> if t == expected then pure () else failI index $ errInvalidOpType name (show expected) (show t)) . opRetType)
  (find (opNameIs name) ctx)
getGrType ctx _ (GFn index name _) expected = maybe
  (error $ errImpossibleCase "getGrType GFn")
  ((\t -> if t == expected then pure () else failI index $ errInvalidFnType name (show expected) (show t)) . fnRetType)
  (find (fnNameIs name) ctx)
getGrType _ locVar (GVar index name) expected = maybe
  (error $ errImpossibleCase "getGrType GVar")
  ((\t -> if t == expected then pure () else failI index $ errInvalidVarType name (show expected) (show t)) . fst)
  (find ((== name) . snd) locVar)
getGrType _ _ (GLit index lit) expected = if isType expected lit then pure () else failI index $ errInvalidLitType (show expected) (show $ getLitType lit)
getGrType _ _ (GGr index _) _ = failI index $ errImpossibleCase "getGrType GGr"

validateMount :: Ctx -> LocalVariable -> Group -> Parser ()
validateMount _ _ (GGr index _) = failI index $ errImpossibleCase "validateMount"
validateMount _ locVar (GVar index name) = maybe (failI index $ errVariableNotBound name) (const $ pure ()) (find ((== name) . snd) locVar)
validateMount _ _ (GLit {}) = pure ()
validateMount ctx locVar (GFn index name args) = case find (fnNameIs name) ctx of
  Nothing -> failI index $ errFunctionNotBound name
  Just (Function _ args' _ _)
    | expected /= found -> failI index $ errInvalidNumberOfArgument name expected found
    | otherwise -> mapM_ (\((t, _), g) -> getGrType ctx locVar g t) (zip args' args) >> traverse_ (validateMount ctx locVar) args
    where
      expected = length args'
      found = length args
  Just _ -> failI index $ errImpossibleCase "validateMount GFn"
validateMount ctx locVar (GOp index name [l, r]) = case find (opNameIs name) ctx of
  Nothing -> failI index $ errOperatorNotBound name
  Just (Operator _ _ _ (tl, _) (tr, _) _) -> getGrType ctx locVar l tl >> getGrType ctx locVar r tr >> validateMount ctx locVar l >> validateMount ctx locVar r
  Just _ -> failI index $ errImpossibleCase "validateMount GOp"
validateMount _ _ (GOp index _ _) = failI index $ errImpossibleCase "validateMount GOp"

opNameIs :: String -> Ast -> Bool
opNameIs name' (Operator {..}) = name' == opName
opNameIs _ _ = False

fnNameIs :: String -> Ast -> Bool
fnNameIs name' (Function {..}) = name' == fnName
fnNameIs _ _ = False

stNameIs :: String -> Ast -> Bool
stNameIs name' (Structure {..}) = name' == structName
stNameIs _ _ = False

toSubexpr :: Group -> Parser SubExpression
toSubexpr (GGr _ _) = fail $ errImpossibleCase "toSubexpr GGr"
toSubexpr (GLit _ lit) = pure $ Lit lit
toSubexpr (GFn _ name body) = FunctionCall name <$> traverse toSubexpr body
toSubexpr (GVar _ name) = pure $ VariableCall name
toSubexpr (GOp _ name body) = FunctionCall name <$> traverse toSubexpr body

subexpression2 :: Ctx -> LocalVariable -> Parser SubExpression
subexpression2 ctx locVar = do
  group <- someTill (getGroup ctx locVar) eof <|> failN errEmptyExpr -- need some errhandling
  mount <- mountGroup ctx group
  validateMount ctx locVar mount
  toSubexpr mount

subexpression :: Ctx -> LocalVariable -> Parser SubExpression
subexpression ctx locVar = do
  group <- someTill (getGroup ctx locVar <|> failN errSemiColon) (tok SemiColon) <|> failN errEmptyExpr
  mount <- mountGroup ctx group
  validateMount ctx locVar mount
  toSubexpr mount

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
            x@(IfThenElse {}) -> (:) x <$> getExprAndUpdateCtx c l r -- should check if both ways return so we end it there
            x@(While {}) -> (:) x <$> getExprAndUpdateCtx c l r -- should check if both ways return so we end it there

getOpeName :: [String] -> Parser String
getOpeName names = tok OperatorKw *> ope >>= notTaken names id

getOpePrec :: Parser Int
getOpePrec = tok PrecedenceKw *> (precValue <|> pure 0)
  where
    precValue = satisfy (\case
      Literal (IntLit _) -> True
      _ -> False) >>= (\case
          (Literal (IntLit x)) -> pure x
          _ -> failN $ errImpossibleCase "getOpePrec precValue")

validArgNumber :: Idx -> String -> [(Type, String)] -> Parser [(Type, String)]
validArgNumber _ _ args@[_, _] = pure args
validArgNumber start name args = do
  offset <- subtract (start - 1) <$> getOffset
  failI start $ errOpArgs offset (length args) name

getConstrTypes :: Ctx -> Parser [Type]
getConstrTypes ctx = do
  t <- types ctx False
  endFound <- (tok Pipe $> False) <|> (tok SemiColon $> True)
  if endFound
    then pure [t]
    else (:) t <$> getConstrTypes ctx

function :: Ctx -> Parser Ast
function ctx = do
  name <- getFnName (getNames ctx)
  args <- getFnArgs ctx (name : getNames ctx)
  retT <- getFnRetType ctx
  let shellFn = Function name args retT []
  body <- getFnBody (shellFn : ctx) args retT
  return $ Function name args retT body

operator :: Ctx -> Parser Ast
operator ctx = do
  name <- getOpeName (getNames ctx)
  prcd <- getOpePrec
  offset <- (+1) <$> getOffset
  args <- getFnArgs ctx (name : getNames ctx) >>= validArgNumber offset name
  retT <- getFnRetType ctx
  let shellOp = Operator name prcd retT (head args) (args !! 1) []
  body <- getFnBody (shellOp : ctx) args retT
  return $ Operator name prcd retT (head args) (args !! 1) body

constraint :: Ctx -> Parser Ast
constraint ctx = do
  name <- satisfy (\case
    (Type ConstraintType {}) -> True
    _ -> False) >>= (\case
      (Type (ConstraintType n _)) -> pure n
      _ -> failN $ errImpossibleCase "constraint")
  void (tok Assign)
  ts <- getConstrTypes ctx
  return $ Constraint name ts

structure :: Ctx -> Parser Ast
structure _ = failN $ errTodo "structure" -- garice
