{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine tests
-}
module Main where

import VM.VirtualMachine

-- stock environment for testing
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

-- basic tests
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

-- run tests
runTests :: IO ()
runTests = do
  putStrLn "Running tests..."

  -- 1: basics
  case exec [] [] basicMathProgram [] of
    Right (IntVal result) ->
      putStrLn $ "Test 1 Passed: Basic Math = " ++ show result
    Left err ->
      putStrLn $ "Test 1 Failed: " ++ err

  -- 2: conditional
  case exec [] [] conditionalProgram [] of
    Right (IntVal result) ->
      putStrLn $ "Test 2 Passed: Conditional = " ++ show result
    Left err ->
      putStrLn $ "Test 2 Failed: " ++ err

  -- 3: factorial
  case exec testEnv [] factorialProgram [] of
    Right (IntVal result) ->
      putStrLn $ "Test 3 Passed: Factorial = " ++ show result
    Left err ->
      putStrLn $ "Test 3 Failed: " ++ err

  -- 4: error handling
  case exec [] [] errorProgram [] of
    Right _ ->
      putStrLn "Test 4 Failed: Error not caught"
    Left err ->
      putStrLn $ "Test 4 Passed: Caught error: " ++ err

  putStrLn "All tests completed."

main :: IO ()
main = runTests