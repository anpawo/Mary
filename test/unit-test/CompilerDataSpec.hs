{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module contains unit tests for the CompilerData component.
-}

module CompilerDataSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (SpecWith)

import Bytecode.Data

spec :: Spec
spec = do
  instructionSpec
  valueSpec
  typeSpec
  lambdaSpec

instructionSpec :: SpecWith ()
instructionSpec = describe "data Value" $ do
  describe "Instruction Show instance" $ do
    it "shows Push correctly" $ show (Push (VmInt 42)) `shouldBe` "Push 42"
    it "shows Call correctly" $ show Call `shouldBe` "Call"
    it "shows Ret correctly" $ show Ret `shouldBe` "Ret"
    it "shows Store correctly" $ show (Store "x") `shouldBe` "Store \"x\""
    it "shows Load correctly" $ show (Load "y") `shouldBe` "Load \"y\""
    it "shows JumpIfFalse correctly" $ show (JumpIfFalse 10) `shouldBe` "JumpIfFalse 10"
    it "shows JumpBackward correctly" $ show (JumpBackward 5) `shouldBe` "JumpBackward 5"

valueSpec :: SpecWith ()
valueSpec = describe "Value Show instance" $ do
  it "shows VmChar correctly" $ show (VmChar 'a') `shouldBe` "a"
  it "shows VmBool correctly" $ show (VmBool True) `shouldBe` "true"
  it "shows VmInt correctly" $ show (VmInt 42) `shouldBe` "42"
  it "shows VmFloat correctly" $ show (VmFloat 3.14) `shouldBe` "3.14"
  it "shows VmString correctly" $ show (VmString "hello") `shouldBe` "hello"
  it "shows VmPreArray correctly" $ show (VmPreArray "int" [[Push (VmInt 1)], [Push (VmInt 2)]]) `shouldBe` "[[Push 1],[Push 2]]"
  it "shows VmPreStruct correctly" $ show (VmPreStruct "test" [("field1", [Push (VmInt 1)])]) `shouldBe` "test{[Push 1]}"
  it "shows VmNull correctly" $ show VmNull `shouldBe` "null"
  it "shows VmFunc correctly" $ show (VmFunc "myFunc") `shouldBe` "function myFunc"
  it "shows VmBool False correctly" $ show (VmBool False) `shouldBe` "false"
  it "shows VmArray correctly" $ show (VmArray "int" [VmInt 1, VmInt 2]) `shouldBe` "[1, 2]"
  it "shows VmClosure correctly" $ show (VmClosure "myClosure") `shouldBe` "closure (myClosure)"
  it "shows VmStruct empty correctly" $ show (VmStruct "empty" []) `shouldBe` "[]"
  it "shows VmStruct 'elem' with one element correctly" $ show (VmStruct "elem" [("data", VmInt 99), ("next", VmStruct "empty" [])]) `shouldBe` "[99]"
  it "shows a normal VmStruct with no fields" $ show (VmStruct "myStruct" []) `shouldBe` "myStruct{}"
  it "shows a normal VmStruct with multiple fields" $ show (VmStruct "myStruct" [("f1", VmInt 1), ("f2", VmString "test")]) `shouldBe` "myStruct{1, test}"
  it "shows VmStruct 'elem' with multiple elements correctly" $
    show (VmStruct "elem"
      [ ("data", VmInt 1)
      , ("next", VmStruct "elem"
          [ ("data", VmInt 2)
          , ("next", VmStruct "empty" [])
          ])
      ])
      `shouldBe` "[1, 2]"
  it "shows VmPreStruct with multiple fields correctly" $
    show (VmPreStruct "myStruct"
      [ ("field1", [Push (VmInt 1)])
      , ("field2", [Push (VmInt 2)])
      ])
      `shouldBe` "myStruct{[Push 1], [Push 2]}"

typeSpec :: SpecWith ()
typeSpec = describe "typeCheck for Value" $ do
  it "returns True for VmNull / \"null\"" $ typeCheck VmNull "null" `shouldBe` True
  it "returns True for VmChar 'a' / \"char\"" $ typeCheck (VmChar 'a') "char" `shouldBe` True
  it "returns True for VmBool False / \"bool\"" $ typeCheck (VmBool False) "bool" `shouldBe` True
  it "returns True for VmInt 42 / \"int\"" $ typeCheck (VmInt 42) "int" `shouldBe` True
  it "returns True for VmFloat / \"float\"" $ typeCheck (VmFloat 3.14) "float" `shouldBe` True
  it "returns True for VmString / \"str\"" $ typeCheck (VmString "ok") "str" `shouldBe` True
  it "returns True for VmArray \"int\" / \"arr[int]\"" $ typeCheck (VmArray "int" [VmInt 1]) "arr[int]" `shouldBe` True
  it "returns True for VmStruct \"myStruct\" [] / \"myStruct\"" $ typeCheck (VmStruct "myStruct" []) "myStruct" `shouldBe` True
  it "returns False for mismatch" $ typeCheck (VmInt 42) "float" `shouldBe` False
  it "returns False for an array type mismatch" $ typeCheck (VmArray "int" [VmInt 1]) "arr[float]" `shouldBe` False
  it "returns False for a struct type mismatch" $ typeCheck (VmStruct "myStruct" []) "anotherStruct" `shouldBe` False
  it "returns True for VmArray \"float\" / \"arr[float]\"" $ typeCheck (VmArray "float" [VmFloat 3.14]) "arr[float]" `shouldBe` True
  it "returns False for mismatch VmArray \"float\" / \"arr[int]\"" $ typeCheck (VmArray "float" [VmFloat 3.14]) "arr[int]" `shouldBe` False
  it "uses EnvVar in a test" $ do
    let env :: EnvVar
        env = ("test", [Call, Ret])
    env `shouldBe` ("test", [Call, Ret])
  it "uses EnvVar type" $ do
    let e :: EnvVar
        e = ("testEnv", [Call, Ret])
    e `shouldBe` ("testEnv", [Call, Ret])
  let testIt :: (TypeCheck a) => a -> String -> Bool
      testIt = typeCheck
  it "uses TypeCheck in a polymorphic context" $
      testIt (VmInt 42) "int" `shouldBe` True

lambdaSpec :: SpecWith ()
lambdaSpec = describe "Value Show instance - Lambda and Void cases" $ do
  it "shows VmVoid correctly" $ 
    show VmVoid `shouldBe` "VmVoid"
  it "shows VmLambda correctly" $ do
    let lambda = VmLambda { lbEnv = [("x", [Push (VmInt 42)])]
                          , lbBody = [Call, Ret]
                          }
    show lambda `shouldBe` "lambda"
  it "checks lbVarCaptured and lbBody for VmPreLambda" $ do
    let preLambda = VmPreLambda { lbVarCaptured = ["var1", "var2"]
                                , lbBody = [Push (VmInt 42)]
                                }
    lbVarCaptured preLambda `shouldBe` ["var1", "var2"]
    lbBody preLambda `shouldBe` [Push (VmInt 42)]
  it "checks lbEnv and lbBody for VmLambda" $ do
    let lambda = VmLambda { lbEnv = [("envVar", [Store "x"])]
                          , lbBody = [Push (VmInt 100)]
                          }
    lbEnv lambda `shouldBe` [("envVar", [Store "x"])]
    lbBody lambda `shouldBe` [Push (VmInt 100)]

  it "shows VmPreLambda correctly with empty captured vars" $ do
      let preLambda = VmPreLambda { lbVarCaptured = []
                                  , lbBody = [Call, Ret]
                                  }
      show preLambda `shouldBe` "lambda"

  it "shows VmPreLambda correctly with captured vars and body" $ do
      let preLambda = VmPreLambda { lbVarCaptured = ["y", "z"]
                                  , lbBody = [Call, Push (VmInt 1), Ret]
                                  }
      show preLambda `shouldBe` "lambda"
