{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module TokenSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, Expectation)
import Test.Hspec.Runner (SpecWith)

import Parser.Token

(==>) :: (Show a, Eq a) => a -> a -> Expectation
(==>) got expected = got `shouldBe` expected

spec :: Spec
spec = do
  subexpressionSpec
  typeSpec
  literalSpec
  identifierSpec
  tokenSpec

tokenSpec :: SpecWith ()
tokenSpec = describe "token" $ do
  describe "show" $ do
    describe "keywords" $ do
      it "function" $ show FunctionKw ==> "function"
      it "operator" $ show OperatorKw ==> "operator"
      it "type" $ show TypeKw ==> "type"
      it "precedence" $ show PrecedenceKw ==> "precedence"
      it "import" $ show ImportKw ==> "import"
      it "if" $ show IfKw ==> "if"
      -- it "then" $ (show $ ThenKw) ==> "then"
      it "else" $ show ElseKw ==> "else"
      it "return" $ show ReturnKw ==> "return"
    describe "symbols" $ do
      it "{" $ show CurlyOpen ==> "{"
      it "}" $ show CurlyClose ==> "}"
      it "(" $ show ParenOpen ==> "("
      it ")" $ show ParenClose ==> ")"
      it "[" $ show BracketOpen ==> "["
      it "]" $ show BracketClose ==> "]"
      it "->" $ show Arrow ==> "->"
      it ";" $ show SemiColon ==> ";"
      it "," $ show Comma ==> ","
      it "|" $ show Pipe ==> "|"
    describe "types" $ do
      it "Type IntType" $ show (Type IntType) ==> "int"
      it "Identifier OperatorId" $ show (Identifier $ OperatorId "<*>") ==> "<*>"

identifierSpec :: SpecWith ()
identifierSpec = describe "identifier" $ do
  describe "show" $ do
    it "add" $ show (TextId "add") ==> "add"
    it "<*>" $ show (OperatorId "<*>") ==> "<*>"

literalSpec :: SpecWith ()
literalSpec = describe "literal" $ do
  describe "show" $ do
    it "'c'" $ show (CharLit 'c') ==> "'c'"
    it "true" $ show (BoolLit True) ==> "true"
    it "false" $ show (BoolLit False) ==> "false"
    it "1" $ show (IntLit 1) ==> "1"
    it "1.0" $ show (FloatLit 1) ==> "1.0"
    it "\"lol\"" $ show (StringLit "lol") ==> "\"lol\""
    it "int [1]" $ show (ArrLitPre IntType [[Literal $ IntLit 1], [Literal $ IntLit 1]]) ==> "int [[[1],[1]]]"
    it "int [1]" $ show (ArrLit IntType [Lit $ IntLit 1, Lit $ IntLit 1]) ==> "int [Lit 1, Lit 1]"
    it "person { name = \"marius\", age = 1 }" $ show (StructLitPre "person" [("name", [Literal $ StringLit "marius"]), ("age", [Literal $ IntLit 1])]) ==> "person { name = \"marius\", age = 1 }"
    it "person { name = \"marius\", age = 1 }" $ show (StructLit "person" [("name", Lit $ StringLit "marius"), ("age", Lit $ IntLit 1)]) ==> "person { name = Lit \"marius\", age = Lit 1 }"

typeSpec :: SpecWith ()
typeSpec = describe "type" $ do
  describe "eq" $ do
    it "struct any == struct person" $ (StructAnyType == StructType "person") ==> True
    it "struct person == struct any" $ (StructType "person" == StructAnyType) ==> True
    it "any == struct any" $ (AnyType == StructAnyType) ==> True
    it "struct any == any" $ (StructAnyType == AnyType) ==> True
    it "char == int" $ (CharType == IntType) ==> False
    it "type number == type integer" $ (ConstraintType (Just "number") [IntType, FloatType] == ConstraintType (Just "integer") [IntType, BoolType]) ==> True
    it "int == type integer" $ (IntType == ConstraintType (Just "integer") [IntType, BoolType]) ==> True
  describe "show" $ do
    it "char" $ show CharType ==> "char"
    it "void" $ show VoidType ==> "void"
    it "bool" $ show BoolType ==> "bool"
    it "int" $ show IntType ==> "int"
    it "float" $ show FloatType ==> "float"
    it "str" $ show StrType ==> "str"
    it "arr[int]" $ show (ArrType IntType) ==> "arr[int]"
    it "struct person" $ show (StructType "person") ==> "struct person"
    it "any" $ show AnyType ==> "any"
    it "struct any" $ show StructAnyType ==> "struct any"
    it "constraint number = int | float" $ show (ConstraintType (Just "number") [IntType, FloatType]) ==> "number"

subexpressionSpec :: SpecWith ()
subexpressionSpec = describe "subexpression" $ do
  it "varCallName" $
    varCallName (VariableCall "test") ==> "test"
  it "fnCallName" $
    fnCallName (FunctionCall "test" []) ==> "test"
  it "fnCallArgs" $
    fnCallArgs (FunctionCall "test" []) ==> []
