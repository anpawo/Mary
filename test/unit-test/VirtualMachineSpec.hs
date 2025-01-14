{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module VirtualMachineSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import VM.VirtualMachine
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
