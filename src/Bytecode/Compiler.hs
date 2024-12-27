{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Compiler bytecode
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use isNothing" #-}
-- to prevent warnings
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Bytecode.Compiler
  (
  ) where

import Ast.Ast

data Instruction
  = Push Value
  | Call
  | Ret
  | Store String
  | Load String

data Value
  = VmInt Int
  | VmBool Bool
  | VmStr String


data EnvVar = EnvVar { envVarName :: String, envVarBody :: [Instruction]}
