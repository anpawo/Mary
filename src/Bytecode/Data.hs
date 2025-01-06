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
    EnvVar(..),

  ) where

data Instruction
  = Push Value
  | Call
  | Ret
  | Store String
  | Load String
  | JumpIfFalse Int
  | JumpBackward Int
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data EnvVar = EnvVar { envVarName :: String, envVarBody :: [Instruction]} deriving (Show, Eq)
