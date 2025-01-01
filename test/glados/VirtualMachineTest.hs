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

-- programs
basicMathProgram :: Program
basicMathProgram =
  [ Push (IntVal 10)
  , Push (IntVal 5)
  , Call Add
  , Ret
  ]

conditionalProgram :: Program
conditionalProgram =
  [ Push (IntVal 10)
  , Push (IntVal 5)
  , Call Less
  , JumpIfFalse 2
  , Push (IntVal 42)
  , Ret
  , Push (IntVal 0)
  , Ret
  ]

factorialProgram :: Program
factorialProgram =
  [ Push (IntVal 5)
  , PushEnv "fact"
  , Call
  , Ret
  ]

errorProgram :: Program
errorProgram =
  [ Push (IntVal 42)
  , Call Add  -- not enough arguments
  , Ret
  ]