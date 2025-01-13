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
      it "import" $ show (ImportKw "std") ==> "import std"
      it "if" $ show IfKw ==> "if"
      it "builtin" $ show BuiltinKw ==> "builtin"
      it "then" $ show ThenKw ==> "then"
      it "else" $ show ElseKw ==> "else"
      it "return" $ show ReturnKw ==> "return"
      it "while" $ show WhileKw ==> "while"

    describe "symbols" $ do
      it "{" $ show CurlyOpen ==> "{"
      it "}" $ show CurlyClose ==> "}"
      it "(" $ show ParenOpen ==> "("
      it ")" $ show ParenClose ==> ")"
      it "[" $ show BracketOpen ==> "["
      it "]" $ show BracketClose ==> "]"
      it "->" $ show Arrow ==> "->"
      it ";" $ show SemiColon ==> ";"
      it ":" $ show Colon ==> ":"
      it "," $ show Comma ==> ","
      it "|" $ show Pipe ==> "|"
      it "=" $ show Assign ==> "="

    describe "types" $ do
      it "Type IntType" $ show (Type IntType) ==> "int"
      it "Type FloatType" $ show (Type FloatType) ==> "float"

    describe "identifier in MyToken" $ do
      it "Identifier (TextId \"abc\")" $ show (Identifier (TextId "abc")) ==> "abc"

identifierSpec :: SpecWith ()
identifierSpec = describe "identifier" $ do
  describe "show" $ do
    it "add" $ show (TextId "add") ==> "add"
    it "<*>" $ show (OperatorId "<*>") ==> "<*>"

  describe "eq/ord" $ do
    it "TextId vs OperatorId" $ do
      (TextId "abc" == OperatorId "abc") `shouldBe` False
      (TextId "abc" < TextId "abd") `shouldBe` True

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
    it "person { name = \"marius\", age = 1 }" $
      show (StructLitPre "person" [("name",[Literal $ StringLit "marius"]), ("age",[Literal $ IntLit 1])])
        ==> "person { name = \"marius\", age = 1 }"
    it "person { name = \"marius\", age = 1 }" $
      show (StructLit "person" [("name", Lit $ StringLit "marius"), ("age", Lit $ IntLit 1)])
        ==> "person { name = Lit \"marius\", age = Lit 1 }"
    it "NULL" $ show NullLit ==> "NULL"
    it "ClosureLit \"myOp\"" $
      show (ClosureLit "myOp" [IntType] BoolType) ==> "(myOp)"

  describe "eq/ord" $ do
    it "CharLit vs BoolLit" $ do
      (CharLit 'a' == BoolLit True) `shouldBe` False
      (CharLit 'a' < CharLit 'b') `shouldBe` True

typeSpec :: SpecWith ()
typeSpec = describe "type" $ do
  describe "eq" $ do
    it "char == int" $ (CharType == IntType) ==> False
    it "type number == type integer" $
      (ConstraintType (Just "number") [IntType, FloatType] == ConstraintType (Just "integer") [IntType, BoolType]) ==> True
    it "int == type integer" $
      (IntType == ConstraintType (Just "integer") [IntType, BoolType]) ==> True
    it "ClosureType [IntType] FloatType == ClosureType [IntType] FloatType -> True" $
      (ClosureType [IntType] FloatType == ClosureType [IntType] FloatType) ==> True
    it "ClosureType [IntType] FloatType == ClosureType [BoolType] FloatType -> False" $
      (ClosureType [IntType] FloatType == ClosureType [BoolType] FloatType) ==> False
    it "AnyType == (ClosureType [IntType] FloatType) -> False" $
      (AnyType == ClosureType [IntType] FloatType) ==> False
    it "AnyType == IntType -> True" $
      (AnyType == IntType) ==> True
    it "(ClosureType [IntType] FloatType) == AnyType -> False" $
      (ClosureType [IntType] FloatType == AnyType) ==> False
    it "BoolType == AnyType -> True" $
      (BoolType == AnyType) ==> True

  describe "show" $ do
    it "char" $ show CharType ==> "char"
    it "null" $ show NullType ==> "null"
    it "void" $ show VoidType ==> "void"
    it "bool" $ show BoolType ==> "bool"
    it "int" $ show IntType ==> "int"
    it "float" $ show FloatType ==> "float"
    it "str" $ show StrType ==> "str"
    it "arr[int]" $ show (ArrType IntType) ==> "arr[int]"
    it "struct person" $ show (StructType "person") ==> "person"
    it "any" $ show AnyType ==> "any"
    it "constraint number = int | float" $ show (ConstraintType (Just "number") [IntType, FloatType]) ==> "number"
    it "constraint with the same name (Just n) == (Just n)" $ do
      let c1 = ConstraintType (Just "number") [IntType, FloatType]
          c2 = ConstraintType (Just "number") [BoolType] 
      (c1 == c2) ==> True
    it "constraint c in list ts" $ do
      let c = ConstraintType Nothing [IntType]
          big = ConstraintType Nothing [c, FloatType]
      (c == big) ==> True
    it "(ConstraintType _ ts) == t => t in ts" $ do (ConstraintType Nothing [IntType, BoolType] == IntType) ==> True
    it "t == (ConstraintType _ ts) => t in ts" $ do (FloatType == ConstraintType Nothing [FloatType, BoolType]) ==> True

subexpressionSpec :: SpecWith ()
subexpressionSpec = describe "subexpression" $ do
  it "varCallName" $
    varCallName (VariableCall "test") ==> "test"
  it "fnCallName" $
    fnCallName (FunctionCall "test" []) ==> "test"
  it "fnCallArgs" $
    fnCallArgs (FunctionCall "test" []) ==> []
  it "SubExpression Eq: same VariableCall" $ do
    (VariableCall "x" == VariableCall "x") ==> True
    (VariableCall "x" == VariableCall "y") ==> False
  it "SubExpression Show: variable call" $ do
    show (VariableCall "abc") ==> "VariableCall {varCallName = \"abc\"}"
  it "SubExpression Eq: function call" $ do
    let f1 = FunctionCall "f" [VariableCall "x"]
        f2 = FunctionCall "f" [VariableCall "x"]
        f3 = FunctionCall "g" []
    (f1 == f2) ==> True
    (f1 == f3) ==> False
  it "SubExpression Ord" $ do
    VariableCall "a" < VariableCall "b" `shouldBe` True
