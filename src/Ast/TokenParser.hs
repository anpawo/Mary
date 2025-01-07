{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TokenParser
-}

module Ast.TokenParser (operatorIdentifier, textIdentifier, constraintType, arrayType, structType, charType, boolType, nullType, intType, floatType, strType, voidType, intLiteral, isLiteral) where 

import Parser.Token

import Text.Megaparsec (Parsec, satisfy, single)

import Data.Void (Void)


type Parser = Parsec Void [MyToken]


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

isLiteral :: MyToken -> Bool
isLiteral (Literal {}) = True
isLiteral _            = False

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
charType = typing <$> single (Type CharType)

boolType :: Parser Type
boolType = typing <$> single (Type BoolType)

nullType :: Parser Type
nullType = typing <$> single (Type NullType)

voidType :: Parser Type
voidType = typing <$> single (Type VoidType)

intType :: Parser Type
intType = typing <$> single (Type IntType)

floatType :: Parser Type
floatType = typing <$> single (Type FloatType)

strType :: Parser Type
strType = typing <$> single (Type StrType)
