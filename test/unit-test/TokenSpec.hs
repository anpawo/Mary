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
      it "atom" $ show AtomKw ==> "atom"
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
    it "ClosureType [IntType,BoolType] -> float" $
      show (ClosureType [IntType, BoolType] FloatType) ==> "(int, bool) -> float"
    it "ConstraintType (Just \"number\") [IntType, FloatType]" $
      show (ConstraintType (Just "number") [IntType, FloatType]) ==> "number"
    it "ConstraintType Nothing [IntType, BoolType]" $
      show (ConstraintType Nothing [IntType, BoolType]) ==> "int | bool"
    it "field usage crTyTypes" $ do
      let c = ConstraintType Nothing [StrType, NullType]
      crTyTypes c ==> [StrType, NullType]
    it "field usage fnTyArgs/fnTyRet" $ do
      let clos = ClosureType [IntType, FloatType] StrType
      fnTyArgs clos ==> [IntType, FloatType]
      fnTyRet clos ==> StrType
  
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
  it "SubExpression Eq: function call" $ do
    let f1 = FunctionCall "f" [VariableCall "x"]
        f2 = FunctionCall "f" [VariableCall "x"]
        f3 = FunctionCall "g" []
    (f1 == f2) ==> True
    (f1 == f3) ==> False
  it "SubExpression Eq: Lit vs VariableCall" $ do
    (Lit (BoolLit True) == VariableCall "x") ==> False
    (Lit (BoolLit True) == Lit (BoolLit True)) ==> True
  it "SubExpression Show: variable call" $ do
    show (VariableCall "abc") ==> "VariableCall {varCallName = \"abc\"}"
  it "SubExpression Show: function call" $ do
    show (FunctionCall "foo" [VariableCall "bar"])
      ==> "FunctionCall {fnCallName = \"foo\", fnCallArgs = [VariableCall {varCallName = \"bar\"}]}"
  it "SubExpression Show: lit" $ do
    show (Lit (IntLit 42)) ==> "Lit 42"
  it "SubExpression Ord: variableCall a < variableCall b" $ do
    VariableCall "a" < VariableCall "b" `shouldBe` True
  it "SubExpression Ord: compare function calls" $ do
    let f1 = FunctionCall "aaa" []
        f2 = FunctionCall "zzz" []
    (f1 < f2) `shouldBe` True

  describe "additional coverage for SubExpression Ord" $ do
    it "compare function calls with different arity" $ do
      let f1 = FunctionCall "f" [VariableCall "x"]
          f2 = FunctionCall "f" [VariableCall "x", VariableCall "y"]
      (f1 < f2) `shouldBe` True
      (f2 < f1) `shouldBe` False
    it "compare function call with variable call" $ do
      let f = FunctionCall "fun" []
          v = VariableCall "var"
      (f < v) `shouldBe` False
      (v < f) `shouldBe` True

eqSpec :: SpecWith ()
eqSpec = describe "additional coverage for Type eq" $ do
  it "StructType different names" $
    (StructType "foo" == StructType "bar") `shouldBe` False
  it "ArrType different inner types" $
    (ArrType IntType == ArrType BoolType) `shouldBe` False
  it "ConstraintType same name, different content" $
    (ConstraintType (Just "num") [IntType] == ConstraintType (Just "num") [FloatType, IntType])
      `shouldBe` True
  it "ConstraintType c@(...) == (ConstraintType _ ts) when c ∉ ts" $
    let c1 = ConstraintType Nothing [BoolType]
        c2 = ConstraintType (Just "maybe") [ConstraintType Nothing [IntType], FloatType]
    in (c1 == c2) `shouldBe` False
  it "ConstraintType c@(...) == (ConstraintType _ ts) when c ∈ ts" $
    let c1 = ConstraintType Nothing [BoolType]
        c2 = ConstraintType (Just "maybe") [c1, FloatType]
    in (c1 == c2) `shouldBe` True
  it "(ConstraintType Nothing [IntType]) == BoolType -> False" $
    (ConstraintType Nothing [IntType] == BoolType) `shouldBe` False
  it "(ConstraintType Nothing [IntType]) == IntType -> True" $
    (ConstraintType Nothing [IntType] == IntType) `shouldBe` True
  it "(ConstraintType (Just \"something\") []) == (ConstraintType (Just \"other\") []) -> False" $
    (ConstraintType (Just "something") [] == ConstraintType (Just "other") []) `shouldBe` False


ordSpec :: SpecWith ()
ordSpec = describe "additional coverage for MyToken eq/ord" $ do
  it "MyToken eq on keywords" $ do
    (FunctionKw == FunctionKw) `shouldBe` True
    (FunctionKw == OperatorKw) `shouldBe` False
  it "MyToken eq on same constructor with different fields" $ do
    (ImportKw "x" == ImportKw "x") `shouldBe` True
    (ImportKw "x" == ImportKw "y") `shouldBe` False
  it "MyToken eq on Type" $ do
    (Type IntType == Type IntType) `shouldBe` True
    (Type IntType == Type BoolType) `shouldBe` False
  it "MyToken eq on Literal" $ do
    (Literal (IntLit 2) == Literal (IntLit 2)) `shouldBe` True
    (Literal (IntLit 2) == Literal (IntLit 3)) `shouldBe` False
  it "MyToken eq on Identifier" $ do
    (Identifier (TextId "a") == Identifier (TextId "a")) `shouldBe` True
    (Identifier (TextId "a") == Identifier (TextId "b")) `shouldBe` False
  it "MyToken ord on keywords" $ do
    (FunctionKw < OperatorKw) `shouldBe` True
    (ReturnKw < IfKw) `shouldBe` False
  it "MyToken ord with mixed constructors" $ do
    (FunctionKw < CurlyOpen) `shouldBe` True
    (ParenClose < ImportKw "abc") `shouldBe` False
