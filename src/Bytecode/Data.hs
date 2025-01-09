{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Data bytecode
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
-- to prevent warnings
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}

module Bytecode.Data
  (
    --main
    Instruction(..),
    Value(..),
    EnvVar,
    TypeCheck(..),
  ) where
import Text.Printf (printf)

data Instruction
  = Push Value
  | Call
  | Ret
  | Store String
  | Load String
  | JumpIfFalse Int
  | JumpBackward Int
  deriving (Eq)

instance Show Instruction where
  show (Push value)      = "Push " ++ show value
  show Call              = "Call"
  show Ret               = "Ret"
  show (Store name)      = "Store " ++ show name
  show (Load name)       = "Load " ++ show name
  show (JumpIfFalse pos) = "JumpIfFalse " ++ show pos
  show (JumpBackward pos)= "JumpBackward " ++ show pos

data Value
  = VmChar Char
  | VmBool Bool
  | VmInt Int
  | VmFloat Double
  | VmString String
  | VmArray String [[Instruction]]
  | VmStruct String [(String, [Instruction])]
  | VmNull
  | VmFunc String
  deriving (Eq)

instance Show Value where
  show (VmChar c)         = [c]
  show (VmBool True)      = "true"
  show (VmBool False)     = "false"
  show VmNull             = "null"
  show (VmInt i)          = show i
  show (VmFloat f)        = show f
  show (VmString s)       = s
  show (VmArray typeName instrs)   = show instrs
  show (VmStruct structName fields)  = printf "%s{%s}" structName $ show fields
  show (VmFunc name)      = printf "function %s" name

type EnvVar = (String, [Instruction])

class TypeCheck a where
  typeCheck :: a -> String -> Bool

instance TypeCheck Value where
  typeCheck :: Value -> String -> Bool
  typeCheck (VmNull {}) "null" = True
  typeCheck (VmChar {}) "char" = True
  typeCheck (VmBool {}) "bool" = True
  typeCheck (VmInt {}) "int" = True
  typeCheck (VmFloat {}) "float" = True
  typeCheck (VmString {}) "str" = True
  typeCheck (VmArray typeName _) expected
    | printf "arr[%s]" typeName == expected = True
    | otherwise = False
  typeCheck (VmStruct structName _) expected
    | structName == expected = True
    | otherwise = False
  typeCheck _ _ = False
