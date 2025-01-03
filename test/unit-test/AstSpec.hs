{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}
{-# LANGUAGE QuasiQuotes #-}

module AstSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, Expectation)
import Test.Hspec.Runner (SpecWith)
import Text.RawString.QQ
import Text.Megaparsec.Error (ParseErrorBundle(..))

import Data.Void (Void)

import Parser.Tokenizer
import Parser.Token
import Ast.Ast
import Utils.Lib

(==>) :: (Show a, Eq a, Show b, Eq b) => Either a b -> b -> Expectation
(==>) got expected = got `shouldBe` Right expected

-- (==!) :: (Show a, Eq a, Show b, Eq b) => Either a b -> a -> Expectation
-- (==!) got expected = got `shouldBe` Left expected

pAst :: String -> Either (ParseErrorBundle [MyToken] Void) [Ast]
pAst s = case run tokenize s of
  Left _ -> error "pAst"
  Right o -> run tokenToAst o

spec :: Spec
spec = do
  functionSpec
  operatorSpec

op0 :: String
op0 = [r|
operator **(int a, int b) -> int {
    return 0;
}
|]

opRec :: String
opRec = [r|
operator ** precedence 8 (int a, int b) -> int {
  return a ** (b - 1);
}
|]

operatorSpec :: SpecWith ()
operatorSpec = describe "operator" $ do
    describe "creation" $ do
      it "basic" $ pAst op0 ==> [Operator {opName = "**", opPrecedence = 0, opRetType = IntType, opArgLeft = (IntType,"a"), opArgRight = (IntType,"b"), opBody = [Return {retValue = Lit (IntLit 0)}]}]
      it "recursive" $ pAst opRec ==> [Operator {opName = "**", opPrecedence = 8, opRetType = IntType, opArgLeft = (IntType,"a"), opArgRight = (IntType,"b"), opBody = [Return {retValue = FunctionCall {fnCallName = "**", fnCallArgs = [VariableCall {varCallName = "a"},FunctionCall {fnCallName = "-", fnCallArgs = [VariableCall {varCallName = "b"},Lit $ IntLit 1]}]}}]}]

fnNoParam :: String
fnNoParam = [r|
function zero() -> int {
    return 0;
}
|]

fnParam :: String
fnParam = [r|
function suc(int x) -> int {
    return x + 1;
}
|]

functionSpec :: SpecWith ()
functionSpec = describe "function" $ do
    describe "creation" $ do
      it "no param" $ pAst fnNoParam ==> [Function {fnName = "zero", fnArgs = [], fnRetType = IntType, fnBody = [Return {retValue = Lit (IntLit 0)}]}]
      it "one param" $ pAst fnParam ==> [Function {fnName = "suc", fnArgs = [(IntType,"x")], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnCallName = "+", fnCallArgs = [VariableCall {varCallName = "x"},Lit (IntLit 1)]}}]}]
