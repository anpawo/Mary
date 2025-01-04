{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module CompilerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (SpecWith)

import Parser.Token (Literal(..), Type(..))
import Bytecode.Compiler
import Ast.Ast

spec :: Spec
spec = do
  convertLiteralSpec
  compileSubExpressionSpec
  compileExpressionSpec
  compileExpressionsSpec
  compileParamSpec
  compileParamsSpec

convertLiteralSpec :: SpecWith ()
convertLiteralSpec = describe "convertLiteral" $ do
  it "converts CharLit to VmChar" $
    convertLiteral (CharLit 'c') `shouldBe` VmChar 'c'
  it "converts BoolLit to VmBool" $
    convertLiteral (BoolLit True) `shouldBe` VmBool True
  it "converts IntLit to VmInt" $
    convertLiteral (IntLit 42) `shouldBe` VmInt 42
  it "converts FloatLit to VmFloat" $
    convertLiteral (FloatLit 3.14) `shouldBe` VmFloat 3.14
  it "converts StringLit to VmString" $
    convertLiteral (StringLit "test") `shouldBe` VmString "test"
  it "converts ArrLit to VmArray" $
    convertLiteral (ArrLit IntType [Lit $ IntLit 1, Lit $ IntLit 2]) `shouldBe` VmArray [Push (VmInt 1), Push (VmInt 2)]
  it "converts StructLit to VmStruct" $
    convertLiteral (StructLit "person" [("name", Lit $ StringLit "marius"), ("age", Lit $ IntLit 1)]) `shouldBe` VmStruct [("name", [Push $ VmString "marius"]), ("age", [Push $ VmInt 1])]

compileSubExpressionSpec :: SpecWith ()
compileSubExpressionSpec = describe "compileSubExpression" $ do
    it "compiles a VariableCall" $
      compileSubExpression (VariableCall "x") `shouldBe` [Load "x"]
    it "compiles a FunctionCall with no arguments" $
      compileSubExpression (FunctionCall "testFunction" []) `shouldBe` [PushEnv "testFunction", Call]
    it "compiles a FunctionCall with arguments" $
      compileSubExpression (FunctionCall "double" [Lit (IntLit 42), Lit (IntLit 2)]) `shouldBe` [Push (VmInt 42), Push (VmInt 2), PushEnv "double", Call]
    it "compiles a Literal" $
      compileSubExpression (Lit (StringLit "hello")) `shouldBe` [Push (VmString "hello")]

compileExpressionSpec :: SpecWith ()
compileExpressionSpec = describe "compileExpression" $ do
  it "compiles a SubExpression" $ 
    compileExpression (SubExpression (Lit (IntLit 42))) `shouldBe` [Push (VmInt 42)]
  it "compiles a Variable" $ 
    compileExpression (Variable (IntType, "x") (Lit (IntLit 10))) `shouldBe` [Push (VmInt 10), Store "x"]
  it "compiles a Return expression" $ 
    compileExpression (Return (Lit (StringLit "result"))) `shouldBe` [Push (VmString "result"), Ret]

compileExpressionsSpec :: SpecWith ()
compileExpressionsSpec = describe "compileExpressions" $ do
  it "compiles a list of expressions" $ 
    let expressions = [SubExpression (Lit (IntLit 1)), Variable (IntType, "x") (Lit (IntLit 2)), Return (Lit (StringLit "done"))]
    in compileExpressions expressions `shouldBe` [Push (VmInt 1), Push (VmInt 2), Store "x", Push (VmString "done"), Ret]

compileParamSpec :: SpecWith ()
compileParamSpec = describe "compileParam" $ do
  it "compiles a parameter into instructions" $ 
    compileParam (IntType, "param") `shouldBe` [Store "param"]

compileParamsSpec :: SpecWith ()
compileParamsSpec = describe "compileParams" $ do
  it "compiles multiple parameters into instructions" $ 
    compileParams [(IntType, "x"), (StrType, "y")] `shouldBe` [Store "y", Store "x"]
