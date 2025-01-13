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
  typeSpec
  derivingSpec

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
  it "shows VmVoid correctly" $ show VmVoid `shouldBe` "void"
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

derivingSpec :: SpecWith ()
derivingSpec = describe "deriving Eq" $ do
  it "checks Eq for VmVoid" $ do
    (VmVoid == VmVoid) `shouldBe` True
    (VmVoid == VmNull) `shouldBe` False
  it "checks equality of two instructions" $ (Push (VmInt 42) == Push (VmInt 42)) `shouldBe` True
  it "checks inequality of two instructions" $ (Push (VmInt 42) == Ret) `shouldBe` False
  it "checks equality of two values" $ (VmInt 42 == VmInt 42) `shouldBe` True
  it "checks inequality of two values" $ (VmInt 42 == VmFloat 42.0) `shouldBe` False
  it "checks Eq for all Instruction constructors" $ do
    (Push (VmInt 1) == Push (VmInt 1)) `shouldBe` True
    (Push (VmInt 1) == Push (VmInt 2)) `shouldBe` False
    (Call == Call) `shouldBe` True
    (Call == Ret)  `shouldBe` False
    (Ret == Ret) `shouldBe` True
    (Store "x" == Store "x") `shouldBe` True
    (Store "x" == Store "y") `shouldBe` False
    (Update "x" == Update "x") `shouldBe` True
    (Update "x" == Update "z") `shouldBe` False
    (Load "a" == Load "a") `shouldBe` True
    (Load "a" == Load "b") `shouldBe` False
    (JumpIfFalse 10 == JumpIfFalse 10) `shouldBe` True
    (JumpIfFalse 10 == JumpIfFalse 20) `shouldBe` False
    (JumpBackward 5 == JumpBackward 5) `shouldBe` True
    (JumpBackward 5 == JumpBackward 1) `shouldBe` False
  it "checks Eq for all Value constructors" $ do
    (VmChar 'a' == VmChar 'a') `shouldBe` True
    (VmChar 'a' == VmChar 'b') `shouldBe` False
    (VmBool True == VmBool True) `shouldBe` True
    (VmBool True == VmBool False) `shouldBe` False
    (VmInt 1 == VmInt 1) `shouldBe` True
    (VmInt 1 == VmInt 2) `shouldBe` False
    (VmFloat 3.14 == VmFloat 3.14) `shouldBe` True
    (VmFloat 3.14 == VmFloat 2.71) `shouldBe` False
    (VmString "hello" == VmString "hello") `shouldBe` True
    (VmString "hello" == VmString "world") `shouldBe` False
    (VmNull == VmNull) `shouldBe` True
    (VmNull == VmInt 42) `shouldBe` False
    (VmFunc "f" == VmFunc "f") `shouldBe` True
    (VmFunc "f" == VmFunc "g") `shouldBe` False
    (VmVoid == VmVoid) `shouldBe` True
    (VmVoid == VmNull) `shouldBe` False
    (VmArray "int" [VmInt 1] == VmArray "int" [VmInt 1]) `shouldBe` True
    (VmArray "int" [VmInt 1] == VmArray "float" [VmFloat 1.0]) `shouldBe` False
    (VmStruct "myStruct" [("field", VmInt 1)] == VmStruct "myStruct" [("field", VmInt 1)]) `shouldBe` True
    (VmStruct "myStruct" [("field", VmInt 1)] == VmStruct "myStruct" [("field", VmInt 2)]) `shouldBe` False
    (VmPreArray "int" [[Push (VmInt 1)]] == VmPreArray "int" [[Push (VmInt 1)]]) `shouldBe` True
    (VmPreArray "int" [[Push (VmInt 1)]] == VmPreArray "int" [[Push (VmInt 2)]]) `shouldBe` False
    (VmPreStruct "test" [("f1", [Push (VmInt 1)])] == VmPreStruct "test" [("f1", [Push (VmInt 1)])]) `shouldBe` True
    (VmPreStruct "test" [("f1", [Push (VmInt 1)])] == VmPreStruct "test" [("f1", [Push (VmInt 2)])]) `shouldBe` False
    (VmClosure "c1" == VmClosure "c1") `shouldBe` True
    (VmClosure "c1" == VmClosure "c2") `shouldBe` False
  it "checks equality of two VmStruct with multiple fields" $ 
    (VmStruct "multi" [("f1", VmInt 1), ("f2", VmBool True)] 
    == VmStruct "multi" [("f1", VmInt 1), ("f2", VmBool True)]) `shouldBe` True
  it "checks inequality of two VmStruct with multiple fields" $ 
    (VmStruct "multi" [("f1", VmInt 1), ("f2", VmBool True)] 
    == VmStruct "multi" [("f1", VmInt 2), ("f2", VmBool True)]) `shouldBe` False
  it "checks Eq between different constructors (closure vs string)" $ do
    (VmClosure "x" == VmString "x") `shouldBe` False
    (VmClosure "x" == VmClosure "x") `shouldBe` True
  it "checks Eq for multiple sub-lists in a VmPreArray" $ do
    (VmPreArray "int" [[Push (VmInt 1)], [Push (VmInt 2)]]
      == VmPreArray "int" [[Push (VmInt 1)], [Push (VmInt 2)]])
        `shouldBe` True
    (VmPreArray "int" [[Push (VmInt 1)], [Push (VmInt 2)]]
      == VmPreArray "int" [[Push (VmInt 1)], [Push (VmInt 3)]])
        `shouldBe` False
