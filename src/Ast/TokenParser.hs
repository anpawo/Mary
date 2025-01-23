{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module provides functionality for parsing tokens in the AST.
-}

module Ast.TokenParser (operatorIdentifier, textIdentifier, constraintType, arrayType, structType, charType, boolType, nullType, intType, floatType, strType, voidType, intLiteral, isLiteral, tok, importKw, isStructLiteral) where 

import Parser.Token

import Text.Megaparsec (Parsec, satisfy, single)

import Data.Void (Void)


type Parser = Parsec Void [MyToken]

-- alias
tok :: MyToken -> Parser MyToken
tok = single

-- identifier check
isSymIdentifier :: MyToken -> Bool
isSymIdentifier (Identifier (TextId {})) = True
isSymIdentifier _                        = False

isOpIdentifier :: MyToken -> Bool
isOpIdentifier (Identifier (OperatorId {})) = True
isOpIdentifier _                            = False

-- type check
isConstraintType :: MyToken -> Bool
isConstraintType (Type (ConstraintType {})) = True
isConstraintType _                          = False

isArrayType :: MyToken -> Bool
isArrayType (Type (ArrType {})) = True
isArrayType _                   = False

isStructType :: MyToken -> Bool
isStructType (Type (StructType {})) = True
isStructType _                   = False

-- literal check
isIntLiteral :: MyToken -> Bool
isIntLiteral (Literal (IntLit {})) = True
isIntLiteral _                     = False

isStructLiteral :: MyToken -> Bool
isStructLiteral (Literal (StructLitPre {})) = True
isStructLiteral _                           = False

isLiteral :: MyToken -> Bool
isLiteral (Literal {}) = True
isLiteral _            = False

-- keyword check
isImport :: MyToken -> Bool
isImport (ImportKw _) = True
isImport _            = False

-- literal parser
intLiteral :: Parser Int
intLiteral = intLiteralValue . literal <$> satisfy isIntLiteral

-- identifier parser
operatorIdentifier :: Parser String
operatorIdentifier = opIdName . identifier <$> satisfy isOpIdentifier

textIdentifier :: Parser String
textIdentifier = textIdName . identifier <$> satisfy isSymIdentifier

-- type parser
constraintType :: Parser Type
constraintType = typing <$> satisfy isConstraintType

arrayType :: Parser Type
arrayType = typing <$> satisfy isArrayType

structType :: Parser Type
structType = typing <$> satisfy isStructType

charType :: Parser Type
charType = typing <$> tok (Type CharType)

boolType :: Parser Type
boolType = typing <$> tok (Type BoolType)

nullType :: Parser Type
nullType = typing <$> tok (Type NullType)

voidType :: Parser Type
voidType = typing <$> tok (Type VoidType)

intType :: Parser Type
intType = typing <$> tok (Type IntType)

floatType :: Parser Type
floatType = typing <$> tok (Type FloatType)

strType :: Parser Type
strType = typing <$> tok (Type StrType)

-- keyword parser
importKw :: Parser MyToken
importKw = satisfy isImport