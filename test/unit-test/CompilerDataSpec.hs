{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module CompilerDataSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (SpecWith)

import Bytecode.Data

spec :: Spec
spec = do
  instructionSpec
  valueSpec

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
    it "shows VmArray correctly" $ show (VmArray "int" [VmInt 1, VmInt 2]) shouldBe` "[1, 2]"
    it "shows VmClosure correctly" $ show (VmClosure "myClosure") `shouldBe` "closure (myClosure)"


