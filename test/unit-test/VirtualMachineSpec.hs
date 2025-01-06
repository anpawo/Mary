{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module VirtualMachineSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import VM.VirtualMachine
  ( exec
  , Value(..)
  , Operator(..)
  , Instruction(..)
  , Program
  , Env
  )

-- env
testEnv :: Env
testEnv =
  [ ("fact", FuncVal
      [ PushArg 0
      , Push (IntVal 1)
      , Push (OpVal Eq)
      , Call
      , JumpIfFalse 2
      , Push (IntVal 1)
      , Ret

      , PushArg 0
      , Push (IntVal 1)
      , Push (OpVal Sub)
      , Call
      , PushEnv "fact"
      , Call
      , PushArg 0
      , Push (OpVal Mul)
      , Call
      , Ret
      ])
  ]

-- tests programs
basicMathProgram :: Program
basicMathProgram =
  [ Push (IntVal 10)
  , Push (IntVal 5)
  , Push (OpVal Add)
  , Call
  , Ret
  ]

conditionalProgram :: Program
conditionalProgram =
  [ Push (IntVal 10)
  , Push (IntVal 5)
  , Push (OpVal Less)
  , Call
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
  , Push (OpVal Add)  -- stack underflow
  , Call
  , Ret
  ]

spec :: Spec
spec = describe "Virtual Machine Tests" $ do

  it "executes basicMathProgram and returns 15" $ do
    let result = exec [] [] basicMathProgram []
    result `shouldBe` Right (IntVal 15)

  it "executes conditionalProgram and returns 0" $ do
    let result = exec [] [] conditionalProgram []
    result `shouldBe` Right (IntVal 0)

  it "executes factorialProgram and returns 120" $ do
    let result = exec testEnv [] factorialProgram []
    result `shouldBe` Right (IntVal 120)

  it "executes errorProgram and fails with error message" $ do
    let result = exec [] [] errorProgram []
    result `shouldBe` Left "Add expects two IntVal on the stack"