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

module Bytecode.Data
  (
    --main
    Instruction(..),
    Value(..),
    EnvVar,
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
  | VmArray [Instruction]
  | VmStruct [(String, [Instruction])]
  | VmNull
  | VmFunc String
  deriving (Eq)

instance Show Value where
  show (VmChar c)         = [c]
  show (VmBool True)      = "true"
  show (VmBool False)     = "false"
  show (VmInt i)          = show i
  show (VmFloat f)        = show f
  show (VmString s)       = s
  show (VmArray instrs)   = printf "[%s]" $ show instrs
  show (VmStruct fields)  = printf "{%s}" $ show fields
  show VmNull             = "null"
  show (VmFunc name)      = printf "function %s" name

type EnvVar = (String, [Instruction])
