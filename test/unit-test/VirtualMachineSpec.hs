{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module VirtualMachineSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import VM.VirtualMachine
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
