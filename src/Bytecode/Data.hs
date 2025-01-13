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
import Data.List (intercalate)

data Instruction
  = Push Value
  | Call
  | Ret
  | Store String
  | Update String
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
  | VmPreArray String [[Instruction]]
  | VmPreStruct String [(String, [Instruction])]
  | VmNull
  | VmFunc String
  | VmVoid
  | VmArray String [Value]
  | VmStruct String [(String, Value)]
  | VmClosure String
  deriving (Eq)

instance Show Value where
  show (VmChar c)                      = [c]
  show (VmBool True)                   = "true"
  show (VmBool False)                  = "false"
  show VmNull                          = "null"
  show (VmInt i)                       = show i
  show (VmFloat f)                     = show f
  show (VmString s)                    = s
  show (VmPreArray typeName instrs)    = show instrs
  show (VmPreStruct structName fields) = printf "%s{%s}" structName $ intercalate ", " $ map (show . snd) fields
  show (VmFunc name)                   = printf "function %s" name
  show (VmArray typeName instrs)       = printf "[%s]" $ intercalate ", " $ map show instrs
  show (VmClosure n)                   = printf "closure (%s)" n
  show (VmStruct "empty" [])           = "[]"
  show (VmStruct "elem" l)             = printf "[%s]" $ formatList l
    where
      formatList [("data", d), ("next", VmStruct "empty" [])] = show d
      formatList [("data", d), ("next", VmStruct "elem" l)] = printf "%s, %s" (show d) (formatList l)
  show (VmStruct structName fields)    = printf "%s{%s}" structName $ intercalate ", " $ map (show . snd) fields
  show VmVoid                          = "void"

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
  typeCheck (VmStruct structName _) expected
    | structName == expected = True
  typeCheck _ _ = False
