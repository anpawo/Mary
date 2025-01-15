{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

{-# LANGUAGE ScopedTypeVariables #-}
module VirtualMachineSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck ()
import Control.Exception ()
import System.IO.Silently (capture_)
import System.Exit (ExitCode(..))
import System.IO.Error ()
import Data.Char (ord)
import Text.Printf ()
import Data.Maybe ()
import Data.List (isInfixOf)
import Text.Megaparsec.Error ()

import Bytecode.Data
import VM.VirtualMachine

runProg :: Program -> Stack -> IO Value
runProg prog stack = exec 0 [] prog stack

dummyEnv :: Env
dummyEnv =
  [ ("f", [Push (VmInt 999)])
  , ("structVar", [Push (VmStruct "st" [("field", VmInt 123)])])
  , ("arrVar", [Push (VmArray "int" [VmInt 1, VmInt 2, VmInt 3])])
  , ("funcPrint", [Store "param1", Ret])
  , ("myFun", [Push (VmInt 77), Ret])
  , ("fun", [Push (VmInt 1), Store "x", Store "y", Ret])
  ]

spec :: Spec
spec = do
  convArrInstrToValSpec
  convStructInstrToValSpec
  countParamFuncSpec
  jumpIfFalseInstrSpec
  pushInstrSpec
  doCurrentInstrSpec
  getcurrentInstrSpec
  execSpec
  callInstrSpec
  builtinOperatorSpec
  arithmeticOperatorSpec
  conversionOperatorSpec

convArrInstrToValSpec :: Spec
convArrInstrToValSpec = describe "convArrInstrToVal" $ do
  it "handles empty list" $
    convArrInstrToVal [] dummyEnv `shouldReturn` []
  it "handles single item" $ do
    let one = [[Push (VmInt 10), Ret]]
    vals <- convArrInstrToVal one dummyEnv
    vals `shouldBe` [VmInt 10]
  it "handles multiple items" $ do
    let arr =
          [ [Push (VmInt 1), Ret]
          , [Push (VmInt 2), Ret]
          ]
    convArrInstrToVal arr dummyEnv `shouldReturn` [VmInt 1, VmInt 2]

convStructInstrToValSpec :: Spec
convStructInstrToValSpec = describe "convStructInstrToVal" $ do
  it "handles empty struct" $
    convStructInstrToVal [] dummyEnv `shouldReturn` []
  it "handles simple struct" $ do
    let s = [("fieldA",[Push (VmInt 100), Ret])]
    convStructInstrToVal s dummyEnv `shouldReturn` [("fieldA", VmInt 100)]
  it "handles multiple fields" $ do
    let s =
          [ ("fA",[Push (VmInt 1), Ret])
          , ("fB",[Push (VmInt 2), Ret])
          ]
    convStructInstrToVal s dummyEnv `shouldReturn` [("fA", VmInt 1), ("fB", VmInt 2)]

countParamFuncSpec :: Spec
countParamFuncSpec = describe "countParamFunc" $ do
  it "no Store => 0" $
    countParamFunc [] 0 `shouldBe` 0
  it "some instructions but no Store => 0" $
    countParamFunc [Push VmVoid, Push (VmInt 1), Load "x"] 0 `shouldBe` 0
  it "count store" $ do
    let prog = [Store "x", Push (VmInt 2), Store "y", Store "z"]
    countParamFunc prog 0 `shouldBe` 3

jumpIfFalseInstrSpec :: Spec
jumpIfFalseInstrSpec = describe "jumpIfFalseInstr" $ do
  it "jumps when False" $ do
    let prog = [JumpIfFalse 2, Push (VmInt 999), Push (VmInt 888)]
    v <- exec 0 [] prog [VmBool False]
    v `shouldBe` VmVoid
  it "no jump when True" $ do
    let prog = [JumpIfFalse 1, Push (VmInt 999)]
    v <- exec 0 [] prog [VmBool True]
    v `shouldBe` VmInt 999
  it "fail if top not bool" $ do
    let prog = [JumpIfFalse 1]
    (exec 0 [] prog [VmInt 1]) `shouldThrow` anyException

pushInstrSpec :: Spec
pushInstrSpec = describe "pushInstr" $ do
  it "pushes normal value" $ do
    let prog = [Push (VmInt 42)]
    v <- runProg prog []
    v `shouldBe` VmInt 42

  it "pushes VmPreArray" $ do
    let arr = [[Push (VmInt 9), Ret], [Push (VmInt 1), Ret]]
        prog = [Push (VmPreArray "int" arr)]
    v <- runProg prog []
    v `shouldBe` VmArray "int" [VmInt 9, VmInt 1]

  it "pushes VmPreStruct" $ do
    let fields = [("f", [Push (VmInt 777), Ret])]
        prog = [Push (VmPreStruct "MySt" fields)]
    v <- runProg prog []
    v `shouldBe` VmStruct "MySt" [("f", VmInt 777)]

doCurrentInstrSpec :: Spec
doCurrentInstrSpec = describe "doCurrentInstr" $ do
  it "Ret with value" $ do
    doCurrentInstr (Just Ret) 0 [] [] [VmInt 999] `shouldReturn` VmInt 999

  it "Ret empty stack => fail" $
    (doCurrentInstr (Just Ret) 0 [] [] []) `shouldThrow` anyException

  it "Update => missing struct => fail" $ do
    let prog = [Update "var"]
    (exec 0 [] prog [VmInt 9, VmString "field"]) `shouldThrow` anyException

  it "Store => new env variable" $ do
    let prog = [Store "newVar"]
    v <- exec 0 [] prog [VmInt 10]
    v `shouldBe` VmInt 10

  it "Load => loads function from env" $ do
    let env = [("myFun", [Push (VmInt 77), Ret])]
        prog = [Load "myFun"]
    v <- exec 0 env prog []
    v `shouldBe` VmInt 77

  it "Load => fail not found" $ do
    let prog = [Load "nope"]
    (exec 0 [] prog []) `shouldThrow` anyException

  it "Call => top of stack not VmFunc => fail" $ do
    let prog = [Call]
    (exec 0 [] prog [VmInt 5]) `shouldThrow` anyException

  it "JumpBackward => negative index => fail or out of range" $ do
    let prog = [JumpBackward 1, Push (VmInt 1)]
    (exec 0 [] prog []) `shouldThrow` anyException

  it "Nothing => end => returns top of stack" $ do
    doCurrentInstr Nothing 0 [] [] [VmInt 10] `shouldReturn` VmInt 10

  it "Nothing => empty stack => returns VmVoid" $
    doCurrentInstr Nothing 0 [] [] [] `shouldReturn` VmVoid

getcurrentInstrSpec :: Spec
getcurrentInstrSpec = describe "getcurrentInstr" $ do
  it "returns instruction if in range" $ do
    let prog = [Push VmVoid, Ret]
    getcurrentInstr 0 prog `shouldBe` Just (Push VmVoid)
    getcurrentInstr 1 prog `shouldBe` Just Ret

  it "returns Nothing if out of range" $ do
    let prog = [Push VmVoid]
    getcurrentInstr 10 prog `shouldBe` Nothing

execSpec :: Spec
execSpec = describe "exec" $ do
  it "runs an entire program returning final stack top" $ do
    let prog =
          [ Push (VmInt 10)
          , Push (VmInt 2)
          , Push (VmFunc "+")
          , Call
          ]
    v <- runProg prog []
    v `shouldBe` VmInt 12

  it "fails with invalid operator in program" $ do
    let prog =
          [ Push (VmInt 10)
          , Push (VmInt 2)
          , Push (VmFunc "concat")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException

callInstrSpec :: Spec
callInstrSpec = describe "callInstr" $ do

  it "calls closureName but fails after" $ do
    let prog = [Load "myClosure", Call]
    (runProg prog []) `shouldThrow` anyException

  it "fails on unknown function" $ do
    let prog = [Push (VmFunc "nope"), Call]
    (runProg prog []) `shouldThrow` anyException

builtinOperatorSpec :: Spec
builtinOperatorSpec = describe "builtinOperator" $ do
  it "handles print" $ do
    let prog = [Push (VmInt 999), Push (VmFunc "print"), Call]
    out <- capture_ (runProg prog [])
    out `shouldSatisfy` isInfixOf "999"

  it "handles exit code 0" $ do
    let prog = [Push (VmInt 0), Push (VmFunc "exit"), Call]
    (runProg prog []) `shouldThrow` (== ExitSuccess)

  it "fails if exit top is not int" $ do
    let prog = [Push (VmFloat 3.14), Push (VmFunc "exit"), Call]
    (runProg prog []) `shouldThrow` anyException

  it "handles set known field" $ do
    let prog =
          [ Push (VmStruct "person" [("age", VmInt 20)])
          , Push (VmString "age")
          , Push (VmInt 55)
          , Push (VmFunc "set")
          , Call
          ]
    v <- runProg prog []
    v `shouldBe` VmStruct "person" [("age", VmInt 55)]

  it "fails if set unknown field" $ do
    let prog =
          [ Push (VmStruct "person" [])
          , Push (VmString "unknownField")
          , Push (VmInt 55)
          , Push (VmFunc "set")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException

  it "handles toString" $ do
    let prog = [Push (VmInt 999), Push (VmFunc "toString"), Call]
    v <- runProg prog []
    v `shouldBe` VmString "999"

  it "handles toChar from int" $ do
    let prog = [Push (VmInt 65), Push (VmFunc "toChar"), Call]
    v <- runProg prog []
    v `shouldBe` VmChar 'A'

  it "handles length with string" $ do
    let prog = [Push (VmString "abcd"), Push (VmFunc "length"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt 4

  it "handles length with array" $ do
    let prog = [Push (VmArray "int" [VmInt 1, VmInt 2]), Push (VmFunc "length"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt 2

  it "handles insert with string" $ do
    let prog =
          [ Push (VmString "test")
          , Push (VmInt 2)
          , Push (VmChar 'X')
          , Push (VmFunc "insert")
          , Call
          ]
    v <- runProg prog []
    v `shouldBe` VmString "teXst"

  it "handles insert with array" $ do
    let prog =
          [ Push (VmArray "int" [VmInt 1, VmInt 2])
          , Push (VmInt 1)
          , Push (VmInt 999)
          , Push (VmFunc "insert")
          , Call
          ]
    v <- runProg prog []
    v `shouldBe` VmArray "int" [VmInt 1, VmInt 999, VmInt 2]

  it "handles append with string" $ do
    let prog = [Push (VmString "abc"), Push (VmChar 'Z'), Push (VmFunc "append"), Call]
    v <- runProg prog []
    v `shouldBe` VmString "abcZ"

  it "handles append with array" $ do
    let prog = [Push (VmArray "int" [VmInt 1]), Push (VmInt 99), Push (VmFunc "append"), Call]
    v <- runProg prog []
    v `shouldBe` VmArray "int" [VmInt 1, VmInt 99]

  it "handles at with string" $ do
    let prog = [Push (VmString "hello"), Push (VmInt 1), Push (VmFunc "at"), Call]
    v <- runProg prog []
    v `shouldBe` VmChar 'e'

  it "handles at with array" $ do
    let prog = [Push (VmArray "int" [VmInt 1, VmInt 2]), Push (VmInt 0), Push (VmFunc "at"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt 1

  it "handles concat string" $ do
    let prog = [Push (VmString "ab"), Push (VmString "cd"), Push (VmFunc "concat"), Call]
    v <- runProg prog []
    v `shouldBe` VmString "abcd"

  it "handles concat array of same type" $ do
    let prog = [Push (VmArray "int" [VmInt 1]), Push (VmArray "int" [VmInt 2]), Push (VmFunc "concat"), Call]
    v <- runProg prog []
    v `shouldBe` VmArray "int" [VmInt 1, VmInt 2]

  it "fails if concat array of different type" $ do
    let prog = [Push (VmArray "int" [VmInt 1]), Push (VmArray "float" [VmFloat 2.2]), Push (VmFunc "concat"), Call]
    (runProg prog []) `shouldThrow` anyException

  it "fails if invalid operator" $ do
    let prog = [Push (VmInt 1), Push (VmInt 2), Push (VmFunc "???"), Call]
    (runProg prog []) `shouldThrow` anyException

arithmeticOperatorSpec :: Spec
arithmeticOperatorSpec = describe "arithmeticOperator" $ do
  it "int plus int" $ do
    let prog = [Push (VmInt 2), Push (VmInt 3), Push (VmFunc "+"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt 5

  it "float minus float" $ do
    let prog = [Push (VmFloat 5.5), Push (VmFloat 3.2), Push (VmFunc "-"), Call]
    v <- runProg prog []
    case v of
      VmFloat f -> f `shouldSatisfy` (\n -> abs (n - 2.3) < 1e-9)
      _         -> expectationFailure "Expected a VmFloat"

  it "fails with invalid operands" $ do
    let prog = [Push (VmBool True), Push (VmInt 1), Push (VmFunc "+"), Call]
    (runProg prog []) `shouldThrow` anyException

conversionOperatorSpec :: Spec
conversionOperatorSpec = describe "conversionOperator" $ do
  it "toInt from char" $ do
    let prog = [Push (VmChar 'a'), Push (VmFunc "toInt"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt (ord 'a')

  it "toFloat from int" $ do
    let prog = [Push (VmInt 42), Push (VmFunc "toFloat"), Call]
    v <- runProg prog []
    v `shouldBe` VmFloat 42.0

  it "toChar from int" $ do
    let prog = [Push (VmInt 97), Push (VmFunc "toChar"), Call]
    v <- runProg prog []
    v `shouldBe` VmChar 'a'

  it "fails toInt from struct" $ do
    let prog = [Push (VmStruct "stuff" []), Push (VmFunc "toInt"), Call]
    (runProg prog []) `shouldThrow` anyException

  it "fails toFloat from array" $ do
    let prog = [Push (VmArray "int" []), Push (VmFunc "toFloat"), Call]
    (runProg prog []) `shouldThrow` anyException

  it "toInt from string parse ok" $ do
    let prog = [Push (VmString "123"), Push (VmFunc "toInt"), Call]
    v <- runProg prog []
    v `shouldBe` VmInt 123

  it "toInt from string parse fail => Null" $ do
    let prog = [Push (VmString "abc"), Push (VmFunc "toInt"), Call]
    v <- runProg prog []
    v `shouldBe` VmNull