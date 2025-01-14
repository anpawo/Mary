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
import Data.Char ()
import Text.Printf ()
import Text.Megaparsec.Error ()
import Bytecode.Data
import VM.VirtualMachine
import Data.List (isInfixOf)

runProg :: Program -> Stack -> IO Value
runProg prog stack = exec 0 [] prog stack

dummyEnv :: Env
dummyEnv =
  [ ("f", [Push (VmInt 999)])
  , ("structVar", [Push (VmStruct "st" [("field", VmInt 123)])])
  , ("arrVar", [Push (VmArray "int" [VmInt 1, VmInt 2, VmInt 3])])
  , ("funcPrint", [Store "param1", Ret])
  , ("someFunc", [ Push (VmInt 42)
                 , Ret
                 ])
  ]

spec :: Spec
spec = do
  convArrInstrToValSpec
  convStructInstrToValSpec
  countParamFuncSpec
  jumpIfFalseInstrSpec
  exitCallFuncSpec
  toIntCallFuncSpec
  toFloatCallFuncSpec
  operatorCallFuncSpec
  callInstrSpec
  pushInstrSpec
  doCurrentInstrSpec
  getcurrentInstrSpec
  execSpec
  operatorExecSpec
  boolOperatorExecSpec
  additionalTestsSpec

convArrInstrToValSpec :: Spec
convArrInstrToValSpec = do
  describe "convArrInstrToVal" $ do
    it "handles empty list" $ do
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
convStructInstrToValSpec = do
  describe "convStructInstrToVal" $ do
    it "handles empty struct" $ do
      convStructInstrToVal [] dummyEnv `shouldReturn` []
    it "handles simple struct" $ do
      let s = [("fieldA", [Push (VmInt 100), Ret])]
      convStructInstrToVal s dummyEnv `shouldReturn` [("fieldA", VmInt 100)]
    it "handles multiple fields" $ do
      let s =
            [ ("fA", [Push (VmInt 1), Ret])
            , ("fB", [Push (VmInt 2), Ret])
            ]
      convStructInstrToVal s dummyEnv `shouldReturn` [("fA", VmInt 1), ("fB", VmInt 2)]

countParamFuncSpec :: Spec
countParamFuncSpec = do
  describe "countParamFunc" $ do
    it "no Store => 0" $ do
      countParamFunc [] 0 `shouldBe` 0
    it "some instructions but no Store => 5" $ do
      countParamFunc [Push VmVoid, Push (VmInt 1), Load "x"] 0 `shouldBe` 0
    it "count store" $ do
      let prog = [Store "x", Push (VmInt 2), Store "y", Store "z"]
      countParamFunc prog 0 `shouldBe` 3

jumpIfFalseInstrSpec :: Spec
jumpIfFalseInstrSpec = do
  describe "jumpIfFalseInstr" $ do
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

exitCallFuncSpec :: Spec
exitCallFuncSpec = do
  describe "exitCallFunc" $ do
    it "exitSuccess with code 0" $ do
      let stack = [VmInt 0]
      (exitCallFunc 0 [] [] stack) `shouldThrow` (== ExitSuccess)
    it "exitWith code 42" $ do
      let stack = [VmInt 42]
      (exitCallFunc 0 [] [] stack) `shouldThrow` (== ExitFailure 42)
    it "fail if top of stack is not an integer" $ do
      let stack = [VmString "notAnInt"]
      (exitCallFunc 0 [] [] stack) `shouldThrow` anyException

toIntCallFuncSpec :: Spec
toIntCallFuncSpec = do
  describe "toIntCallFunc" $ do
    it "VmChar 'a' -> 97" $ do
      v <- toIntCallFunc 0 [] [] [VmChar 'a']
      v `shouldBe` VmInt 97
    it "VmFloat 3.9 -> 3" $ do
      v <- toIntCallFunc 0 [] [] [VmFloat 3.9]
      v `shouldBe` VmInt 3
    it "VmString \"123\" -> 123" $ do
      v <- toIntCallFunc 0 [] [] [VmString "123"]
      v `shouldBe` VmInt 123
    it "VmString \"xyz\" -> VmNull" $ do
      v <- toIntCallFunc 0 [] [] [VmString "xyz"]
      v `shouldBe` VmNull
    it "fail if empty" $ do
      (toIntCallFunc 0 [] [] []) `shouldThrow` anyException
    it "fail if top of stack is not compatible" $ do
      let notCompatible = [VmStruct "something" []]
      (toIntCallFunc 0 [] [] notCompatible) `shouldThrow` anyException

toFloatCallFuncSpec :: Spec
toFloatCallFuncSpec = do
  describe "toFloatCallFunc" $ do
    it "VmBool True -> 1.0" $ do
      v <- toFloatCallFunc 0 [] [] [VmBool True]
      v `shouldBe` VmFloat 1.0
    it "VmInt 5 -> 5.0" $ do
      v <- toFloatCallFunc 0 [] [] [VmInt 5]
      v `shouldBe` VmFloat 5.0
    it "VmString \"3.14\" -> 3.14" $ do
      v <- toFloatCallFunc 0 [] [] [VmString "3.14"]
      v `shouldBe` VmFloat 3.14
    it "VmString \"notFloat\" -> VmNull" $ do
      v <- toFloatCallFunc 0 [] [] [VmString "notFloat"]
      v `shouldBe` VmNull
    it "fail if empty" $ do
      (toFloatCallFunc 0 [] [] []) `shouldThrow` anyException
    it "fail if top of stack is not compatible" $ do
      let notCompatible = [VmStruct "bad" []]
      (toFloatCallFunc 0 [] [] notCompatible) `shouldThrow` anyException

operatorCallFuncSpec :: Spec
operatorCallFuncSpec = do
  describe "operatorCallFunc" $ do
    it "handles + with 2 ints" $ do
      let stack = [VmInt 3, VmInt 4]
      v <- operatorCallFunc "+" 0 [] [] stack
      v `shouldBe` VmInt 7
    it "handles + with 2 floats" $ do
      let stack = [VmFloat 1.2, VmFloat 3.4]
      v <- operatorCallFunc "+" 0 [] [] stack
      v `shouldBe` VmFloat 4.6
    it "handles / with floats, not zero" $ do
      let stack = [VmFloat 1, VmFloat 4]
      v <- operatorCallFunc "/" 0 [] [] stack
      v `shouldBe` VmFloat 4.0
    it "fail on / by zero int" $ do
      let stack = [VmInt 0, VmInt 10]
      (operatorCallFunc "/" 0 [] [] stack) `shouldThrow` anyException
    it "fail on / by zero float" $ do
      let stack = [VmFloat 0.0, VmFloat 99.9]
      (operatorCallFunc "/" 0 [] [] stack) `shouldThrow` anyException
    it "handles < with 2 ints" $ do
      let stack = [VmInt 5, VmInt 10]
      v <- operatorCallFunc "<" 0 [] [] stack
      v `shouldBe` VmBool False
    it "handles == with 2 values" $ do
      let stack = [VmInt 3, VmInt 3]
      v <- operatorCallFunc "==" 0 [] [] stack
      v `shouldBe` VmBool True
    it "handles . with (VmString fieldName : VmStruct ...)" $ do
      let stack = [VmString "xyz", VmStruct "st" [("xyz", VmInt 2024)]]
      v <- operatorCallFunc "." 0 [] [] stack
      v `shouldBe` VmInt 2024
    it "handles unknown" $ do
      let stack = [VmInt 42]
      (operatorCallFunc "nope" 0 [] [] stack) `shouldThrow` anyException
    it "fail if not enough arguments on stack" $ do
      let stack = [VmInt 3]
      (operatorCallFunc "+" 0 [] [] stack) `shouldThrow` anyException
    it "fail if operator \".\" stack mismatch" $ do
      let stack = [VmInt 999]
      (operatorCallFunc "." 0 [] [] stack) `shouldThrow` anyException

callInstrSpec :: Spec
callInstrSpec = do
  describe "callInstr" $ do
    it "handles set" $ do
      let prog = [Push (VmStruct "person" [("age", VmInt 20)]), Push (VmString "age"), Push (VmInt 30), Push (VmFunc "set"), Call]
      v <- runProg prog []
      v `shouldBe` VmStruct "person" [("age", VmInt 30)]
    it "handles is" $ do
      let prog = [Push (VmInt 3), Push (VmString "int"), Push (VmFunc "is"), Call]
      v <- runProg prog []
      v `shouldBe` VmBool True
    it "handles print" $ do
      let prog = [Push (VmInt 999), Push (VmFunc "print"), Call]
      out <- capture_ (runProg prog [])
      out `shouldSatisfy` isInfixOf "999"
    it "handles length with string" $ do
      let prog = [Push (VmString "abcd"), Push (VmFunc "length"), Call]
      v <- runProg prog []
      v `shouldBe` VmInt 4
    it "handles length with array" $ do
      let prog = [Push (VmArray "int" [VmInt 1, VmInt 2]), Push (VmFunc "length"), Call]
      v <- runProg prog []
      v `shouldBe` VmInt 2
    it "handles toChar with int" $ do
      let prog = [Push (VmInt 65), Push (VmFunc "toChar"), Call]
      v <- runProg prog []
      v `shouldBe` VmChar 'A'
    it "handles unknown call => operatorCallFunc or fail" $ do
      let prog = [Push (VmInt 123), Push (VmFunc "someUnknown"), Call]
      (runProg prog []) `shouldThrow` anyException
    it "fail if set on unknown field" $ do
      let prog =
            [ Push (VmStruct "person" [("age", VmInt 20)])
            , Push (VmString "unknownField")
            , Push (VmInt 999)
            , Push (VmFunc "set")
            , Call
            ]
      (runProg prog []) `shouldThrow` anyException
    it "fail when concat two arrays of different type" $ do
      let prog =
            [ Push (VmArray "int" [VmInt 1])
            , Push (VmArray "float" [VmFloat 2.5])
            , Push (VmFunc "concat")
            , Call
            ]
      (runProg prog []) `shouldThrow` anyException

pushInstrSpec :: Spec
pushInstrSpec = do
  describe "pushInstr" $ do
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
doCurrentInstrSpec = do
  describe "doCurrentInstr" $ do
    it "Ret with value" $ do
      doCurrentInstr (Just Ret) 0 [] [] [VmInt 999] `shouldReturn` VmInt 999
    it "Ret empty stack => fail" $ do
      (doCurrentInstr (Just Ret) 0 [] [] []) `shouldThrow` anyException
    it "Update => missing struct => fail" $ do
      let prog = [Update "var"]
      (exec 0 [] prog [VmInt 9, VmString "field"]) `shouldThrow` anyException
    it "Store => new env variable" $ do
      let prog = [Store "newVar"]
      v <- exec 0 [] prog [VmInt 10]
      v `shouldBe` VmInt 10
    it "Load => loads function from env" $ do
      let e = [("myFun",[Push (VmInt 77), Ret])]
          prog = [Load "myFun"]
      v <- exec 0 e prog []
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
    it "Nothing => empty stack => returns VmVoid" $ do
      doCurrentInstr Nothing 0 [] [] [] `shouldReturn` VmVoid

getcurrentInstrSpec :: Spec
getcurrentInstrSpec = do
  describe "getcurrentInstr" $ do
    it "returns instruction if in range" $ do
      let prog = [Push VmVoid, Ret]
      getcurrentInstr 0 prog `shouldBe` Just (Push VmVoid)
      getcurrentInstr 1 prog `shouldBe` Just Ret
    it "returns Nothing if out of range" $ do
      let prog = [Push VmVoid]
      getcurrentInstr 10 prog `shouldBe` Nothing

execSpec :: Spec
execSpec = do
  describe "exec" $ do
    it "runs an entire program returning final stack top" $ do
      let prog =
            [ Push (VmInt 10)
            , Push (VmInt 2)
            , Push (VmFunc "+")
            , Call
            , Ret
            ]
      v <- runProg prog []
      v `shouldBe` VmInt 12

operatorExecSpec :: Spec
operatorExecSpec = do
  describe "operatorExec" $ do
    it "int plus int" $ do
      let stack = [VmInt 1, VmInt 2]
      v <- operatorExec "+" (+) 0 [] [] stack
      v `shouldBe` VmInt 3
    it "float minus float" $ do
      let stack = [VmFloat 3.5, VmFloat 5.2]
      v <- operatorExec "-" (-) 0 [] [] stack
      case v of
        VmFloat f -> f `shouldSatisfy` (\n -> abs (n - 1.7) < 1e-9)
        _         -> expectationFailure "Expected a VmFloat"
    it "fail if different types" $ do
      let stack = [VmInt 1, VmChar 'c']
      (operatorExec "+" (+) 0 [] [] stack) `shouldThrow` anyException

boolOperatorExecSpec :: Spec
boolOperatorExecSpec = do
  describe "boolOperatorExec" $ do
    it "int < int" $ do
      let stack = [VmInt 3, VmInt 10]
      v <- boolOperatorExec "<" (<) 0 [] [] stack
      v `shouldBe` VmBool False
    it "float > float" $ do
      let stack = [VmFloat 2.1, VmFloat 5.4]
      v <- boolOperatorExec ">" (>) 0 [] [] stack
      v `shouldBe` VmBool True
    it "fail if different types" $ do
      let stack = [VmInt 1, VmString "X"]
      (boolOperatorExec "<" (<) 0 [] [] stack) `shouldThrow` anyException
    it "fail if top of stack is not numbers" $ do
      let stack = [VmInt 1, VmString "abc"]
      (boolOperatorExec "<" (<) 0 [] [] stack) `shouldThrow` anyException

additionalTestsSpec :: Spec
additionalTestsSpec = describe "additional coverage" $ do
  it "exitCallFunc top of stack not int" $ do
    let stack = [VmString "notAnInt"]
    (exitCallFunc 0 [] [] stack) `shouldThrow` anyException
  it "toIntCallFunc top of stack not compatible" $ do
    let s = [VmStruct "something" []]
    (toIntCallFunc 0 [] [] s) `shouldThrow` anyException
  it "toFloatCallFunc top of stack not compatible" $ do
    let s = [VmStruct "bad" []]
    (toFloatCallFunc 0 [] [] s) `shouldThrow` anyException
  it "operatorCallFunc not enough args" $ do
    let st = [VmInt 3]
    (operatorCallFunc "+" 0 [] [] st) `shouldThrow` anyException
  it "operatorCallFunc dot stack mismatch" $ do
    let st = [VmInt 999]
    (operatorCallFunc "." 0 [] [] st) `shouldThrow` anyException
  it "callInstr set on unknown field" $ do
    let prog =
          [ Push (VmStruct "person" [("age", VmInt 20)])
          , Push (VmString "unknownField")
          , Push (VmInt 999)
          , Push (VmFunc "set")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "callInstr concat different array type" $ do
    let prog =
          [ Push (VmArray "int" [VmInt 1])
          , Push (VmArray "float" [VmFloat 2.5])
          , Push (VmFunc "concat")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "callInstr toString" $ do
    let prog =
          [ Push (VmInt 999)
          , Push (VmFunc "toString")
          , Call
          ]
    v <- runProg prog []
    v `shouldBe` VmString "999"
  it "append single argument" $ do
    let prog =
          [ Push (VmChar 'X')
          , Push (VmFunc "append")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "Update => variable not struct => fail" $ do
    let envBad = [("nonStructVar",[Push (VmInt 123)])]
        prog = [Update "nonStructVar"]
    (exec 0 envBad prog [VmInt 999, VmString "field"]) `shouldThrow` anyException
  it "operatorExec plus single argument" $ do
    let stack = [VmInt 10]
    (operatorExec "+" (+) 0 [] [] stack) `shouldThrow` anyException
  it "boolOperatorExec single argument" $ do
    let stack = [VmFloat 3.14]
    (boolOperatorExec ">" (>) 0 [] [] stack) `shouldThrow` anyException
  it "jumpIfFalse empty stack" $ do
    let prog = [JumpIfFalse 1]
    (exec 0 [] prog []) `shouldThrow` anyException
  it "JumpBackward bigger index" $ do
    let prog = [Push (VmInt 1), JumpBackward 10, Push (VmInt 2)]
    (exec 0 [] prog []) `shouldThrow` anyException
  it "callInstr handle at with single argument" $ do
    let prog =
          [ Push (VmInt 0)
          , Push (VmFunc "at")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "callInstr handle append single argument for array" $ do
    let prog =
          [ Push (VmArray "int" [VmInt 1])
          , Push (VmFunc "append")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "callInstr handle toChar with invalid type" $ do
    let prog =
          [ Push (VmFloat 3.14)
          , Push (VmFunc "toChar")
          , Call
          ]
    (runProg prog []) `shouldThrow` anyException
  it "callInstr handle unknown closureName" $ do
    let envWithClosure = [("myClosure",[Push (VmClosure "nope")])]
        prog =
          [Load "myClosure", Call]
    (exec 0 envWithClosure prog []) `shouldThrow` anyException
  it "pushInstr for invalid PreStruct data" $ do
    let fields = [("testField",[JumpIfFalse 2])]
        prog = [Push (VmPreStruct "SomeStruct" fields)]
    (runProg prog []) `shouldThrow` anyException
  it "pushInstr for invalid PreArray data" $ do
    let arr = [[JumpBackward 1]]
        prog = [Push (VmPreArray "int" arr)]
    (runProg prog []) `shouldThrow` anyException
  it "operatorCallFunc not enough elements for /" $ do
    let stack = [VmInt 2]
    (operatorCallFunc "/" 0 [] [] stack) `shouldThrow` anyException
  it "boolOperatorExec with single element stack" $ do
    let stack = [VmInt 10]
    (boolOperatorExec "<" (<) 0 [] [] stack) `shouldThrow` anyException
  it "countParamFunc with multiple non-store" $ do
    countParamFunc [Push (VmInt 1), Push (VmFloat 2.2), Update "x", Load "y"] 0 `shouldBe` 0
  it "jumpIfFalseInstr with empty stack" $ do
    let prog = [JumpIfFalse 2, Push (VmInt 7)]
    (exec 0 [] prog []) `shouldThrow` anyException
  it "exitCallFunc fail if top is float" $ do
    let st = [VmFloat 0.0]
    (exitCallFunc 0 [] [] st) `shouldThrow` anyException
  it "Any unknown function in operatorCallFunc" $ do
    let st = [VmInt 1, VmInt 2]
    (operatorCallFunc "unknownOp" 0 [] [] st) `shouldThrow` anyException
  it "closureName path for callInstr but fail after" $ do
    let envWithClosure = [("cl",[Push (VmClosure "someop")])]
        st = [VmInt 1, VmFunc "cl"]
    (callInstr "cl" 0 envWithClosure [] st) `shouldThrow` anyException

  it "name ++ \" expects two numbers on the stack\"" $ do
    let st = [VmInt 1]
    (operatorExec "someOp" (*) 0 [] [] st) `shouldThrow` anyException 

  it "case find ((== field) . fst) value of / Nothing branch" $ do
    let envVar = ("myStructVar",[Push (VmStruct "someStruct" [("fieldA", VmInt 1)])])
        env' = [envVar]
        st = [VmInt 999, VmString "unknownField", VmStruct "someStruct" [("fieldA", VmInt 1)]]
        prog = [Update "myStructVar"]
    (exec 0 env' prog st) `shouldThrow` anyException

  it "Nothing -> fail $ printf \"Structure '%s' doesn't have the field '%s'.\" name' field" $ do
    let envVar = ("myStruct",[Push (VmStruct "someStruct" [("age", VmInt 20)])])
        env' = [envVar]
        st = [VmInt 50, VmString "unknownField", VmStruct "someStruct" [("age",VmInt 20)]]
        prog = [Update "myStruct"]
    (exec 0 env' prog st) `shouldThrow` anyException

  it "(\"Variable \" ++ name ++ \" is not a structure\")" $ do
    let envVar = ("myVar",[Push (VmInt 10)])
        env' = [envVar]
        st = [VmInt 999, VmString "fieldName"]
        prog = [Update "myVar"]
    (exec 0 env' prog st) `shouldThrow` anyException

  it "(\"Variable \" ++ name ++ \" not found\")" $ do
    let st = [VmInt 123, VmString "field"]
        prog = [Update "unknownVar"]
    (exec 0 [] prog st) `shouldThrow` anyException

  it "(name, [Push v]) : env)" $ do
    let st = [VmInt 42]
        prog = [Store "newVar"]
    v <- exec 0 [] prog st
    v `shouldBe` VmInt 42

  it "drop (countParamFunc body 0) stack)" $ do
    let envFun = ("fun",[Store "x", Store "y", Ret])
        env' = [envFun]
        st = [VmInt 10, VmInt 11, VmInt 999]
        prog = [Load "fun"]
    v <- exec 0 env' prog st
    v `shouldBe` VmInt 10

  it "(\"Variable or function \" ++ name ++ \" not found\")" $ do
    let prog = [Load "nope"]
    (exec 0 [] prog []) `shouldThrow` anyException

  it "stack" $ do
    let st = [VmInt 123, VmInt 456]
    st `shouldBe` [VmInt 123, VmInt 456]

  it "env" $ do
    let e = [("foo",[Push (VmInt 1)]),("bar",[Push (VmInt 2)])]
    length e `shouldBe` 2

  it "ind" $ do
    let i = 42 :: Int
    i `shouldBe` 42

  it "exec 0 env body stack >>= \\res -> exec (ind + 1) env is (res : drop (countParamFunc body 0) stack)" $ do
    let envFun = ("fn",[Store "param1", Ret])
        env' = [envFun]
        st = [VmInt 99]
        prog = [Load "fn", Push (VmInt 123)]
    v <- exec 0 env' prog st
    v `shouldBe` VmInt 123

  it "toFloatCallFunc ind env is stack" $ do
    let st = [VmInt 7]
    v <- toFloatCallFunc 0 [] [] st
    v `shouldBe` VmFloat 7.0

  it "toIntCallFunc ind env is stack" $ do
    let st = [VmFloat 3.9]
    v <- toIntCallFunc 0 [] [] st
    v `shouldBe` VmInt 3

  it "exitCallFunc ind env is stack" $ do
    let st = [VmInt 0]
    (exitCallFunc 0 [] [] st) `shouldThrow` anyException 

  it "exec (ind + 1) env is (VmChar c:stack)" $ do
    let st = [VmChar 'z', VmInt 100]
        prog = [Call]
    (exec 0 [] prog st) `shouldThrow` anyException

  it "\"Cannot concat two arrays of different type\"" $ do
    let st = [VmArray "int" [VmInt 1], VmArray "float" [VmFloat 2.2], VmFunc "concat"]
        prog = [Call]
    (runProg prog st) `shouldThrow` anyException

  it "(n, v')" $ do
    let x = ("someName", VmInt 123)
    fst x `shouldBe` "someName"
    snd x `shouldBe` VmInt 123

  it "printf \"Cannot access field `%s` of struct `%s`.\" fieldName name" $ do
    let st = [VmString "nope", VmStruct "abc" [("xyz", VmInt 1)], VmFunc "."]
        prog = [Call]
    (runProg prog st) `shouldThrow` anyException

  it "\"Invalid function/operator call.\"" $ do
    let st = [VmInt 1, VmInt 2]
    (operatorCallFunc "nope" 0 [] [] st) `shouldThrow` anyException

  it "_ -> fail \". expected a structure to access value\"" $ do
    let st = [VmString "field", VmBool True]
    (operatorCallFunc "." 0 [] [] st) `shouldThrow` anyException

  it "Just [Push (VmStruct _ fields)] -> case lookup fieldName fields of / Just val -> exec (ind + 1) env is (val : rest)" $ do
    let envVar = ("someVar",[Push (VmStruct "abc" [("fX", VmInt 55)])])
        e = [envVar]
        st = [VmString "fX", VmString "someVar"]
    (operatorCallFunc "." 0 e [] st) `shouldReturn` VmInt 55

  it "fail $ printf \"Cannot access field `%s` of struct `%s`.\" fieldName name" $ do
    let st = [VmString "unknown", VmStruct "someSt" []]
    (operatorCallFunc "." 0 [] [] st) `shouldThrow` anyException

  it "rest" $ do
    let rest = [VmBool True, VmInt 5]
    length rest `shouldBe` 2

  it "boolOperatorExec \">\" (>) ind env is stack" $ do
    let st = [VmInt 3, VmInt 10]
    v <- boolOperatorExec ">" (>) 0 [] [] st
    v `shouldBe` VmBool(True)

  it "exec (ind + 1) env is (VmInt (div b a) : stack)" $ do
    let st = [VmInt 2, VmInt 8]
    v <- operatorCallFunc "/" 0 [] [] st
    v `shouldBe` VmInt 4

  it "\"Division by 0 is prohibited for floats\"" $ do
    let st = [VmFloat 0.0, VmFloat 9.9]
    (operatorCallFunc "/" 0 [] [] st) `shouldThrow` anyException

  it "\"Division by 0 is prohibited for integers\"" $ do
    let st = [VmInt 0, VmInt 10]
    (operatorCallFunc "/" 0 [] [] st) `shouldThrow` anyException

  it "operatorExec \"*\" (*) ind env is stack" $ do
    let st = [VmInt 3, VmInt 4]
    v <- operatorCallFunc "*" 0 [] [] st
    v `shouldBe` VmInt 12

  it "operatorExec \"-\" (-) ind env is stack" $ do
    let st = [VmFloat 2.2, VmFloat 5.5]
    v <- operatorCallFunc "-" 0 [] [] st
    v `shouldBe` VmFloat 3.3

  it "\"+\"" $ do
    let st = [VmInt 6, VmInt 7]
    v <- operatorCallFunc "+" 0 [] [] st
    v `shouldBe` VmInt 13

  it "\"error with toInt func\"" $ do
    let st = [VmStruct "nope" []]
    (toIntCallFunc 0 [] [] st) `shouldThrow` anyException

  it "exec (ind + 1) env is (VmFloat v:stack)" $ do
    let st = [VmFloat 6.6]
    v <- toFloatCallFunc 0 [] [] st
    v `shouldBe` VmFloat 6.6

  it "exec (ind + 1) env is (VmInt v:stack)" $ do
    let st = [VmInt 99]
    v' <- toIntCallFunc 0 [] [] st
    v' `shouldBe` VmInt 99

  it "exec (ind + 1) env is (VmInt (fromEnum v):stack)" $ do
    let st = [VmBool True]
    v' <- toIntCallFunc 0 [] [] st
    v' `shouldBe` VmInt 1

  it "\"error with exit func\"" $ do
    let st = [VmBool True]
    (exitCallFunc 0 [] [] st) `shouldThrow` anyException

  it "\"JumpIfFalse expects a boolean on the stack\"" $ do
    let prog = [JumpIfFalse 1]
    (exec 0 [] prog [VmInt 1]) `shouldThrow` anyException

  it "[]" $ do
    ([] :: [Int]) `shouldBe` []

  it "operatorExec expects two numbers on the stack" $ do
    let st = [VmInt 3]
    (operatorExec "+" (+) 0 [] [] st) `shouldThrow` anyException

  it "update struct with non-existing field" $ do
    let envVar = ("structVar", [Push (VmStruct "someStruct" [("fieldA", VmInt 1)])])
        env' = [envVar]
        st = [VmInt 99, VmString "fieldB", VmStruct "someStruct" [("fieldA", VmInt 1)]]
        prog = [Update "structVar"]
    (exec 0 env' prog st) `shouldThrow` anyException

  it "update non-struct variable" $ do
    let envVar = ("var", [Push (VmInt 10)])
        env' = [envVar]
        st = [VmInt 99, VmString "fieldName"]
        prog = [Update "var"]
    (exec 0 env' prog st) `shouldThrow` anyException

  it "update unknown variable" $ do
    let st = [VmInt 99, VmString "field"]
        prog = [Update "unknownVar"]
    (exec 0 [] prog st) `shouldThrow` anyException

  it "call function with parameter cleanup" $ do
    let envFun = ("myFunc", [Store "x", Store "y", Ret])
        env' = [envFun]
        st = [VmInt 10, VmInt 20, VmInt 999]
        prog = [Load "myFunc"]
    v <- exec 0 env' prog st
    v `shouldBe` VmInt 10

  it "convert int to float using toFloatCallFunc" $ do
    let st = [VmInt 42]
    v <- toFloatCallFunc 0 [] [] st
    v `shouldBe` VmFloat 42.0

  it "convert float to int using toIntCallFunc" $ do
    let st = [VmFloat 3.14]
    v <- toIntCallFunc 0 [] [] st
    v `shouldBe` VmInt 3

  it "exit with code 0" $ do
    let st = [VmInt 0]
    (exitCallFunc 0 [] [] st) `shouldThrow` anyException

  it "executes with VmChar on the stack" $ do
    let st = [VmChar 'a', VmInt 99]
        prog = [Call]
    (exec 0 [] prog st) `shouldThrow` anyException

  it "concatenate two arrays of the same type" $ do
    let st = [VmArray "int" [VmInt 1], VmArray "int" [VmInt 2]]
        prog = [Push (VmFunc "concat"), Call]
    v <- exec 0 [] prog st
    v `shouldBe` VmArray "int" [VmInt 1, VmInt 2]

  it "fail to concatenate arrays of different types" $ do
    let st = [VmArray "int" [VmInt 1], VmArray "float" [VmFloat 2.2]]
        prog = [Push (VmFunc "concat"), Call]
    (exec 0 [] prog st) `shouldThrow` anyException

  it "execute field access on struct" $ do
    let envVar = ("structVar", [Push (VmStruct "structName" [("fieldA", VmInt 99)])])
        env' = [envVar]
        st = [VmString "fieldA", VmStruct "structName" [("fieldA", VmInt 99)]]
    v <- operatorCallFunc "." 0 env' [] st
    v `shouldBe` VmInt 99

  it "fail to access non-existing field in struct" $ do
    let st = [VmString "fieldB", VmStruct "structName" [("fieldA", VmInt 99)]]
    (operatorCallFunc "." 0 [] [] st) `shouldThrow` anyException

  it "execute division by zero for integers" $ do
    let st = [VmInt 0, VmInt 10]
    (operatorCallFunc "/" 0 [] [] st) `shouldThrow` anyException

  it "execute division by zero for floats" $ do
    let st = [VmFloat 0.0, VmFloat 10.0]
    (operatorCallFunc "/" 0 [] [] st) `shouldThrow` anyException

  it "execute integer division" $ do
    let st = [VmInt 2, VmInt 10]
    v <- operatorCallFunc "/" 0 [] [] st
    v `shouldBe` VmInt 5

  it "execute float division" $ do
    let st = [VmFloat 2.0, VmFloat 10.0]
    v <- operatorCallFunc "/" 0 [] [] st
    v `shouldBe` VmFloat 5.0

  it "execute subtraction" $ do
    let st = [VmFloat 3.5, VmFloat 5.0]
    v <- operatorExec "-" (-) 0 [] [] st
    v `shouldBe` VmFloat 1.5

  it "convert boolean to integer" $ do
    let st = [VmBool True]
    v <- toIntCallFunc 0 [] [] st
    v `shouldBe` VmInt 1
