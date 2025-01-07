{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TokenToAst
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ast.Ast (Ast(..), tokenToAst, Expression(..), SubExpression(..)) where

import Text.Printf (printf)
import Text.Megaparsec (Parsec, single, eof, satisfy, MonadParsec(..), getOffset, optional, someTill, choice)

import Data.Void (Void)
import Data.Maybe (fromJust, isNothing)
import Data.Functor (($>))
import Data.List (find)
import Data.Foldable (traverse_)

import Control.Applicative ((<|>), Alternative(..))
import Control.Monad (void, unless, when)

import Parser.Token
import Ast.Error
import Ast.TokenParser
import Utils.Lib

type Parser = Parsec Void [MyToken]

data Expression
  = SubExpression SubExpression
  | Variable { varMeta :: (Type, String), varValue :: SubExpression } -- variable creation and modification inside a function
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

type Ctx = [Ast]

builtinType :: String -> Type
builtinType name = case find ((== name) . fst) ts of
  Nothing -> error $ printf "error: this function should only be called in builtins, '%s' not found " name
  Just (_, t) -> ConstraintType (Just name) t
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
    -- io/struct/arr/type functions
    , Operator {opName = ".", opPrecedence = 10, opRetType = AnyType, opArgLeft = (StructAnyType, "l"), opArgRight = (StrType, "r"), opBody = []} -- access structure element (for now we would do: <person>."<name>")
    , Function {fnName = "print", fnArgs = [(ArrType AnyType, "args")], fnRetType = VoidType, fnBody = []}                                        -- display something on stdout
    , Function {fnName = "eprint", fnArgs = [(ArrType AnyType, "args")], fnRetType = VoidType, fnBody = []}                                       -- display something on stderr
    , Function {fnName = "getline", fnArgs = [], fnRetType = StrType, fnBody = []}                                                                -- get a string from stdin
    , Function {fnName = "exit", fnArgs = [(IntType, "status")], fnRetType = VoidType, fnBody = []}                                               -- stops the program with a return type
    , Function {fnName = "length", fnArgs = [(ArrType AnyType, "arr")], fnRetType = IntType, fnBody = []}                                         -- return the length of an array
    , Function {fnName = "at", fnArgs = [(ArrType AnyType, "arr"), (IntType, "index")], fnRetType = AnyType, fnBody = []}                         -- get the element at a certain index
    , Function {fnName = "is", fnArgs = [(AnyType, "x"), (StrType, "type")], fnRetType = BoolType, fnBody = []}                                   -- compare the type of 2 variables
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

type LocalVariable = [(Type, String)]
type RetType = Type

expression :: Ctx -> LocalVariable -> RetType -> Parser Expression
expression ctx locVar retT = choice
  [ exprReturn ctx locVar retT
  , exprVariable ctx locVar
  , exprSubexpr ctx locVar retT
  , exprIf ctx locVar retT
  , exprWhile ctx locVar retT
  ]

getBlock :: Ctx -> LocalVariable -> RetType -> Parser [Expression]
getBlock ctx locVar retT = do
  void (tok CurlyOpen)
  expressions <- some (expression ctx locVar retT)
  void (tok CurlyClose)
  return expressions

exprIf :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprIf ctx locVar retT = do
  void (tok IfKw)
  cond <- subexpression ctx locVar (tok ThenKw)
  condType <- getType ctx locVar cond
  unless (condType == BoolType) $ fail $ errCondNotBool "if"
  thenExpr <- getBlock ctx locVar retT
  maybeElseExpr <- optional $ do
    void (tok ElseKw)
    getBlock ctx locVar retT
  return $ case maybeElseExpr of
    Just elseExpr -> IfThenElse cond thenExpr elseExpr
    Nothing -> IfThenElse cond thenExpr []

exprWhile :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprWhile ctx locVar retT = do
  void (tok WhileKw)
  cond <- subexpression ctx locVar  (tok ThenKw)
  condType <- getType ctx locVar cond
  unless (condType == BoolType) $ fail $ errCondNotBool "while"
  body <- getBlock ctx locVar retT
  return $ While cond body

exprReturn :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprReturn _ _ VoidType = failN errVoidRet
exprReturn ctx locVar retT = do
  void (tok ReturnKw)
  offset <- getOffset
  subexpr <- subexpression ctx locVar (tok SemiColon)
  case subexpr of
    (VariableCall x) -> case find (\(_, n) -> n == x) locVar of
        Just (t, _)
          | t == retT -> return $ Return subexpr -- todo: check return type, todo: prevent missing return at the end
          | otherwise -> failI offset $ errRetType (show retT) (show t)
        Nothing -> failN $ errImpossibleCase "exprReturn variable call"
    (FunctionCall {fnCallName = name}) -> case find (\case (Operator {..}) -> opName == name;(Function {..}) -> fnName == name;_ -> False) ctx of
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

exprVariable :: Ctx -> LocalVariable -> Parser Expression
exprVariable ctx locVar = variableCreation ctx locVar <|> variableAssignation ctx locVar

variableCreation :: Ctx -> LocalVariable -> Parser Expression
variableCreation ctx locVar = do
  t <- types ctx False True
  n <- symbolIdentifier
  void (tok Assign)
  x <- subexpression ctx locVar (tok SemiColon)
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failP $ errAssignType n (show t) (show t')

symbolIdentifier :: Parser String
symbolIdentifier = extractName <$> satisfy isSymbolId
  where
    isSymbolId (Identifier (TextId {})) = True
    isSymbolId _                          = False

    extractName (Identifier (TextId name)) = name
    extractName _                            = fail $ errImpossibleCase "extractName"

variableAssignation :: Ctx -> LocalVariable -> Parser Expression
variableAssignation ctx locVar = do
  n <- symbolIdentifier
  t <- case find ((== n) . snd) locVar of
    Nothing -> fail $ errVariableNotBound n
    Just (t, _) -> pure t
  void (tok Assign)
  x <- subexpression ctx locVar (tok SemiColon)
  t' <- getType ctx locVar x
  if t == t'
    then return $ Variable (t, n) x
    else failP $ errAssignType n (show t) (show t')


exprSubexpr :: Ctx -> LocalVariable -> RetType -> Parser Expression
exprSubexpr ctx locVar _ = SubExpression <$> subexpression ctx locVar (tok SemiColon)

data Group
  = GGr { getIndex :: Int, ggValue :: [Group]}
  | GLit { getIndex :: Int, glValue :: Literal}
  | GFn { getIndex :: Int, gfnName :: String, gfnArgs :: [Group]}
  | GVar { getIndex :: Int, gvarName :: String}
  | GOp { getIndex :: Int, gopName :: String, gopArgs :: [Group]}
  deriving (Show)

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
      Parser.Token.Identifier (TextId _) -> True
      _ -> False) >>= (\case
          (Parser.Token.Identifier (TextId x)) -> pure x
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

getType :: Ctx -> LocalVariable -> SubExpression -> Parser Type
getType _ locVar (VariableCall name) = maybe (fail $ errVariableNotBound name) (pure . fst) (find ((== name) . snd) locVar)
getType ctx _ (FunctionCall name _) = case find (\a -> (isFn a || isOp a) && getName a == name) ctx of
  Just (Function {..}) -> pure fnRetType
  Just (Operator {..}) -> pure opRetType
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
isType StructAnyType (StructLit _ _) = True
isType (ConstraintType _ t) lit = any (`isType` lit) t
isType VoidType _ = False
isType AnyType _ = True
isType NullType NullLit = True
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
getLitType NullLit = NullType

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
fixOp (GOp index "." []: _:_) = failI index errExpectedField
fixOp (x:xs) = (x:) <$> fixOp xs

subexpression :: Ctx -> LocalVariable -> Parser a -> Parser SubExpression
subexpression ctx locVar endSubexpr =
  someTill (getGroup ctx locVar) endSubexpr <|> failN errEmptyExpr >>= fixOp >>= mountGroup ctx >>= \m -> validateMount ctx locVar m *> toSubexpr m

getFnBody :: Ctx -> LocalVariable -> RetType -> Parser [Expression]
getFnBody ctx locVar retT = (tok CurlyOpen <|> failN errStartBody) *> getExprAndUpdateCtx ctx locVar retT
  where
    getExprAndUpdateCtx c l r = (tok CurlyClose $> []) <|> (expression c l r >>= \case
      x@(Variable metadata _) -> (x:) <$> getExprAndUpdateCtx c (metadata : l) r
      x -> (x:) <$> getExprAndUpdateCtx c l r)

validArgNumber :: Int -> String -> [(Type, String)] -> Parser [(Type, String)]
validArgNumber _ _ args@[_, _] = pure args
validArgNumber start name args = getOffset >>= (\offset -> failI start $ errOpArgs offset (length args) name) . subtract (start - 1)

notTaken :: [String] -> String -> Parser String
notTaken names name
  | name `elem` names = fail $ errNameTaken name
  | otherwise = pure name

getFnArgs :: Ctx -> [String] -> Parser [(Type, String)]
getFnArgs ctx names = tok ParenOpen *> (tok ParenClose $> [] <|> args names)
  where
    args n = (,) <$> types ctx False True <*> (textIdentifier >>= notTaken n) >>= \a -> (tok ParenClose $> [a]) <|> (tok Comma *> ((a :) <$> args (snd a : n)))

function :: Ctx -> Parser Ast
function ctx = do
  name <- tok FunctionKw *> textIdentifier >>= notTaken (getNames ctx)
  args <- getFnArgs ctx (name : getNames ctx)
  retT <- (tok Arrow <|> failN errMissingRetT) *> types ctx True True
  body <- getFnBody (Function name args retT [] : ctx) args retT
  return $ Function name args retT body

operator :: Ctx -> Parser Ast
operator ctx = do
  name <- tok OperatorKw *> operatorIdentifier >>= notTaken (getNames ctx)
  prcd <- (tok PrecedenceKw *> intLiteral) <|> pure 0
  args <- getOffset >>= (\offset -> getFnArgs ctx (name : getNames ctx) >>= validArgNumber offset name) . (+1)
  retT <- (tok Arrow <|> failN errMissingRetT) *> types ctx True True
  body <- getFnBody (Operator name prcd retT (head args) (args !! 1) [] : ctx) args retT
  return $ Operator name prcd retT (head args) (args !! 1) body

constraint :: Ctx -> Parser Ast
constraint ctx = Constraint <$> (constraintType >>= notTaken (getNames ctx) . fromJust . crTyName) <*> (tok Assign *> constrTypes)
  where
    constrTypes = types ctx False False >>= \t -> (tok SemiColon $> [t]) <|> (tok Pipe *> ((t:) <$> constrTypes))

structure :: Ctx -> Parser Ast
structure _ = failN $ errTodo "structure"
