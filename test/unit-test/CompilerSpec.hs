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
import Bytecode.Data
import Ast.Ast

spec :: Spec
spec = do
  convertLiteralSpec
  compileSubExpressionSpec
  compileExpressionSpec
  compileExpressionsSpec
  compileParamSpec
  compileParamsSpec
  astToEnvVarSpec
  findMainFuncSpec
  compilerSpec

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
  it "converts NullLit to VmNull" $
    convertLiteral NullLit `shouldBe` VmNull

compileSubExpressionSpec :: SpecWith ()
compileSubExpressionSpec = describe "compileSubExpression" $ do
    it "compiles a VariableCall" $
      compileSubExpression (VariableCall "x") `shouldBe` [Load "x"]
    it "compiles a FunctionCall with no arguments" $
      compileSubExpression (FunctionCall "testFunction" []) `shouldBe` [Push $ VmFunc "testFunction", Call]
    it "compiles a FunctionCall with arguments" $
      compileSubExpression (FunctionCall "double" [Lit (IntLit 42), Lit (IntLit 2)]) `shouldBe` [Push (VmInt 42), Push (VmInt 2), Push $ VmFunc "double", Call]
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
  it "compiles a IfThenElse expression" $ do
    let cond = IfThenElse {ifCond = FunctionCall {fnCallName = "<", fnCallArgs = [VariableCall {varCallName = "a"},VariableCall {varCallName = "b"}]},
    thenExpr = [Return {retValue = VariableCall {varCallName = "a"}}],
    elseExpr = [Return {retValue = VariableCall {varCallName = "b"}}]}
    compileExpression cond `shouldBe` [Load "a",Load "b",Push (VmFunc "<"),Call,JumpIfFalse 2,Load "a",Ret,Load "b",Ret]

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

astToEnvVarSpec :: SpecWith ()
astToEnvVarSpec = describe "astToEnvVar" $ do
  it "astToEnvVar a Function" $ do
    let ast = Function {fnName = "mul_add", fnArgs = [(IntType,"a"),(IntType,"b"),(IntType,"c")], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnCallName = "+", fnCallArgs = [VariableCall {varCallName = "a"},FunctionCall {fnCallName = "*", fnCallArgs = [VariableCall {varCallName = "b"},VariableCall {varCallName = "c"}]}]}}]}
    astToEnvVar ast `shouldBe` Right ("mul_add",[Store "c",Store "b",Store "a",Load "a",Load "b",Load "c",Push (VmFunc "*"),Call,Push (VmFunc "+"),Call,Ret])
  it "astToEnvVar a Operator" $ do
    let ast = Operator {opName = "**", opPrecedence = 8, opRetType = IntType, opArgLeft = (IntType, "n"), opArgRight = (IntType, "power"), opBody = [Return {retValue = FunctionCall {fnCallName = "**", fnCallArgs = [VariableCall {varCallName = "n"}, FunctionCall {fnCallName = "-", fnCallArgs = [VariableCall {varCallName = "power"}, Lit (IntLit 1)]}]}}]}
    astToEnvVar ast `shouldBe` Right ("**",[Store "power",Store "n",Load "n",Load "power",Push (VmInt 1),Push (VmFunc "-"),Call,Push (VmFunc "**"),Call,Ret])
  it "astToEnvVar doesn't work" $
    astToEnvVar (Constraint "test" [IntType, FloatType]) `shouldBe` Left ("Unsupported AST to bytecode: " ++ show (Constraint "test" [IntType, FloatType]))

findMainFuncSpec :: SpecWith ()
findMainFuncSpec = describe "findMainFunc" $ do
  it "check if there is a function main in env" $
    findMainFunc [("lol", [Push (VmInt 1)]), ("main", [Push (VmInt 1)])] `shouldBe` True
  it "check if there isn't a function main in env" $
    findMainFunc [("lol", [Push (VmInt 1)]), ("boobakaka", [Push (VmInt 1)])] `shouldBe` False

compilerSpec :: SpecWith ()
compilerSpec = describe "compiler" $ do
  it "compiler with main" $ do
    let ast = [Function {fnName = "mul_add", fnArgs = [(IntType,"a"),(IntType,"b"),(IntType,"c")], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnCallName = "+", fnCallArgs = [VariableCall {varCallName = "a"},FunctionCall {fnCallName = "*", fnCallArgs = [VariableCall {varCallName = "b"},VariableCall {varCallName = "c"}]}]}}]},Function {fnName = "test_condition", fnArgs = [(IntType,"a"),(IntType,"b")], fnRetType = IntType, fnBody = [IfThenElse {ifCond = FunctionCall {fnCallName = "<", fnCallArgs = [VariableCall {varCallName = "a"},VariableCall {varCallName = "b"}]}, thenExpr = [Return {retValue = VariableCall {varCallName = "a"}}], elseExpr = [Return {retValue = VariableCall {varCallName = "b"}}]}]},Function {fnName = "main", fnArgs = [], fnRetType = IntType, fnBody = [Variable {varMeta = (IntType,"res"), varValue = FunctionCall {fnCallName = "mul_add", fnCallArgs = [Lit (IntLit 1), Lit (IntLit 2), Lit (IntLit 3)]}},Return {retValue = VariableCall {varCallName = "res"}}]}]
    let expected = [("mul_add",[Store "c",Store "b",Store "a",Load "a",Load "b",Load "c",Push (VmFunc "*"),Call,Push (VmFunc "+"),Call,Ret]),("test_condition",[Store "b",Store "a",Load "a",Load "b",Push (VmFunc "<"),Call,JumpIfFalse 2,Load "a",Ret,Load "b",Ret]),("main",[Push (VmInt 1),Push (VmInt 2),Push (VmInt 3),Push (VmFunc "mul_add"),Call,Store "res",Load "res",Ret])]
    compiler ast `shouldBe` Right ([Push $ VmFunc "main", Call], expected)
  it "compiler without main" $ do
    let ast = [Function {fnName = "mul_add", fnArgs = [(IntType,"a"),(IntType,"b"),(IntType,"c")], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnCallName = "+", fnCallArgs = [VariableCall {varCallName = "a"},FunctionCall {fnCallName = "*", fnCallArgs = [VariableCall {varCallName = "b"},VariableCall {varCallName = "c"}]}]}}]},Function {fnName = "test_condition", fnArgs = [(IntType,"a"),(IntType,"b")], fnRetType = IntType, fnBody = [IfThenElse {ifCond = FunctionCall {fnCallName = "<", fnCallArgs = [VariableCall {varCallName = "a"},VariableCall {varCallName = "b"}]}, thenExpr = [Return {retValue = VariableCall {varCallName = "a"}}], elseExpr = [Return {retValue = VariableCall {varCallName = "b"}}]}]}]
    let expected = [("mul_add",[Store "c",Store "b",Store "a",Load "a",Load "b",Load "c",Push (VmFunc "*"),Call,Push (VmFunc "+"),Call,Ret]),("test_condition",[Store "b",Store "a",Load "a",Load "b",Push (VmFunc "<"),Call,JumpIfFalse 2,Load "a",Ret,Load "b",Ret])]
    compiler ast `shouldBe` Right ([], expected)
