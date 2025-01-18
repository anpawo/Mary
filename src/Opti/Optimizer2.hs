{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Optimizer2
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Opti.Optimizer2 (optimizeBytecode) where

import Bytecode.Data
import VM.VirtualMachine (Insts, Env)

optimizeBytecode :: Env -> Env
optimizeBytecode [] = []
optimizeBytecode ((name, insts):rst) = (name, optimizeInsts insts) : optimizeBytecode rst

optimizeInsts :: Insts -> Insts
optimizeInsts [] = []
optimizeInsts (Call:Ret:rst) = TailCall : optimizeInsts rst
optimizeInsts (x:rst) = x : optimizeInsts rst
