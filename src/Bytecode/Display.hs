{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module handles the display of bytecode.
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

module Bytecode.Display
  (
    displayBytecode
  ) where

import Bytecode.Data (EnvVar, Instruction)

formatEnvVar :: EnvVar -> String
formatEnvVar (name, instructions) = name ++ " = \n" ++ unlines (map (("\t" ++) . show) instructions)

displayBytecode :: [Instruction] -> [EnvVar] -> IO ()
displayBytecode [] env = putStrLn ("Program:\nThere is no main" ++ "\n\nEnv:\n" ++ concatMap formatEnvVar env)
displayBytecode instr env = putStrLn ("Program:\n" ++ unlines (map show instr) ++ "\n\nEnv:\n" ++ concatMap formatEnvVar env)
