{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine tests
-}

module Main where

import VM.VirtualMachine

-- env
testEnv :: Env
testEnv =
  [ ("fact", FuncVal
      [ PushArg 0
      , Push (IntVal 1)
      , Call Eq
      , JumpIfFalse 2
      , Push (IntVal 1)
      , Ret
      , PushArg 0
      , Push (IntVal 1)
      , Call Sub
      , PushEnv "fact"
      , Call
      , PushArg 0
      , Call Mul
      , Ret
      ])
  ]