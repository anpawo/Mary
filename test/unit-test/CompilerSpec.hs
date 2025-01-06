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
  findMainFuncSpec

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
    convertLiteral (NullLit) `shouldBe` VmNull

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
    let cond = IfThenElse {ifCond = FunctionCall {fnCallName = "<", fnCallArgs = [VariableCall {varCallName = "a"},VariableCall {varCallName = "b"}]}, thenExpr = [Return {retValue = VariableCall {varCallName = "a"}}], elseExpr = [Return {retValue = VariableCall {varCallName = "b"}}]}
    compileExpression (cond) `shouldBe` [Load "a",Load "b",Push (VmFunc "<"),Call,JumpIfFalse 2,Load "a",Ret,Load "b",Ret]

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

-- astToEnvVar :: Ast -> Either String EnvVar
-- astToEnvVar (Function fnName fnArgs _ fnBody) = Right   (fnName ,(compileParams fnArgs ++ compileExpressions fnBody))
-- astToEnvVar (Operator opName _ _ opArgLeft opArgRight opBody) = Right  (opName ,(compileParams [opArgLeft,opArgRight] ++ compileExpressions opBody))
-- astToEnvVar other = Left $ "Unsupported AST to bytecode: " ++ show other

findMainFuncSpec :: SpecWith ()
findMainFuncSpec = describe "findMainFunc" $ do
  it "check if there is a function main in env" $ 
    findMainFunc [("lol", [Push (VmInt 1)]), ("main", [Push (VmInt 1)])] `shouldBe` True
  it "check if there isn't a function main in env" $ 
    findMainFunc [("lol", [Push (VmInt 1)]), ("boobakaka", [Push (VmInt 1)])] `shouldBe` False

-- compiler :: [Ast] -> Either String ([Instruction], [EnvVar])
-- compiler asts =
--   case mapM astToEnvVar asts of
--     Left err -> Left err
--     Right envVars -> if findMainFunc envVars
--       then Right ([Push $ VmFunc "main", Call], envVars)
--       else Right ([], envVars)