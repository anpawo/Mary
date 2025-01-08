{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}
{-# LANGUAGE QuasiQuotes #-}

module AstSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, Expectation, shouldSatisfy)
import Test.Hspec.Runner (SpecWith)
import Text.RawString.QQ
import Text.Megaparsec.Error (ParseErrorBundle(..))
import Data.Either (isLeft)
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
  ifThenElseSpec
  whileSpec
  structureSpec

structureSpec :: SpecWith ()
structureSpec = describe "structure parsing" $ do
  describe "valid cases" $ do
    it "parses a valid structure with multiple members" $ do
      let input = "struct elem { data: any; next: null | elem; }"
      pAst input ==>
        [ Structure "elem"
            [ ("data", AnyType)
            , ("next", ConstraintType Nothing [NullType, StructType "elem"])
            ]
        ]
    it "parses a valid structure with a single member" $ do
      let input = "struct node { value: int; }"
      pAst input ==>
        [ Structure "node"
            [ ("value", IntType)
            ]
        ]

  describe "invalid cases" $ do
    it "fails on missing semicolon between members" $ do
      let input = "struct elem { data: any next: null | elem; }"
      pAst input `shouldSatisfy` isLeft

    it "fails on missing braces in structure definition" $ do
      let input = "struct elem data: any; next: null | elem;"
      pAst input `shouldSatisfy` isLeft

    it "fails on invalid type for a member" $ do
      let input = "struct elem { data: invalid_type; }"
      pAst input `shouldSatisfy` isLeft

ifThenElseSpec :: SpecWith ()
ifThenElseSpec = describe "if-then-else" $ do
  describe "valid cases" $ do
    it "parses a valid if-then-else statement" $ do
      let input = "function main() -> void { int x = 0; int y = 0; if x < 10 then { y = y + 1; } else { y = 0; } }"
      pAst input ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            , Variable (IntType, "y") (Lit (IntLit 0))
            , IfThenElse (FunctionCall "<" [VariableCall "x", Lit (IntLit 10)])
                        [Variable (IntType, "y") (FunctionCall "+" [VariableCall "y", Lit (IntLit 1)])]
                        [Variable (IntType, "y") (Lit (IntLit 0))]
            ]
        ]
    it "parses a valid if-then statement without else" $ do
      let input = "function main() -> void { int x = 0; int y = 0; if x == 0 then { y = 1; } }"
      pAst input ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            , Variable (IntType, "y") (Lit (IntLit 0))
            , IfThenElse (FunctionCall "==" [VariableCall "x", Lit (IntLit 0)])
                        [Variable (IntType, "y") (Lit (IntLit 1))]
                        []
            ]
        ]
  describe "invalid cases" $ do
    it "fails on missing 'then' keyword in if-then-else" $ do
      let input = "function main() -> void { int x = 0; int y = 0; if x == 0 { y = 1; } else { y = 0; } }"
      pAst input `shouldSatisfy` isLeft

    it "fails on missing braces in if-then-else" $ do
      let input = "function main()  -> void { int x = 0; int y = 0; if x == 0 then y = 1; else y = 0; }"
      pAst input `shouldSatisfy` isLeft


whileSpec :: SpecWith ()
whileSpec = describe "while loop" $ do
  describe "valid cases" $ do
    it "parses a valid while loop with one statement" $ do
      let input = "function main () -> void { int x = 0; while x < 10 then { x = x + 1; } }"
      pAst input ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            ,  While (FunctionCall "<" [VariableCall "x", Lit (IntLit 10)])
                    [Variable (IntType, "x") (FunctionCall "+" [VariableCall "x", Lit (IntLit 1)])]
            ]
        ]
    it "parses a valid while loop with multiple statements" $ do
      let input = "function main () -> void { int x = 0; int y = 0; while x < 10 then { x = x + 1; y = y + 2; } }"
      pAst input ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            , Variable (IntType, "y") (Lit (IntLit 0))
            , While (FunctionCall "<" [VariableCall "x", Lit (IntLit 10)])
                    [ Variable (IntType, "x") (FunctionCall "+" [VariableCall "x", Lit (IntLit 1)])
                    , Variable (IntType, "y") (FunctionCall "+" [VariableCall "y", Lit (IntLit 2)])
                    ]
            ]
        ]
  describe "invalid cases" $ do
    it "fails on missing braces in while loop" $ do
      let input = "function main () -> void { int x = 0; while x < 10 then x = x + 1; }"
      pAst input `shouldSatisfy` isLeft

    it "fails on empty body for while loop" $ do
      let input = "function main () -> void { int x = 0; while x < 10 then {} }"
      pAst input `shouldSatisfy` isLeft


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
