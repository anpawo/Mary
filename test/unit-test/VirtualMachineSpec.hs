{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module VirtualMachineSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import VM.VirtualMachine
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
