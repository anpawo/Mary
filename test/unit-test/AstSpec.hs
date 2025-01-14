{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module AstSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, Expectation, shouldSatisfy, SpecWith, runIO)
import Test.Hspec.Runner ()
import Text.RawString.QQ
import Text.Megaparsec ()
import Text.Megaparsec.Error (ParseErrorBundle(..))
import Data.Either (isLeft)
import Data.Void (Void)

import Parser.Tokenizer
import Parser.Token
import Ast.Ast
import Ast.DeclarationParser
import Ast.Parser
import Utils.Lib
import Utils.ArgParser
import Ast.Import (resolveImports)

(==>) :: (Show a, Eq a, Show b, Eq b) => Either a b -> b -> Expectation
(==>) got expected = got `shouldBe` Right expected

-- (==!) :: (Show a, Eq a, Show b, Eq b) => Either a b -> a -> Expectation
-- (==!) got expected = got `shouldBe` Left expected

pAst :: String -> IO (Either (ParseErrorBundle [MyToken] Void) [Ast])
pAst s = case run tokenize s of
  Left _ -> error "pAst"
  Right (_, tokens) -> do
    (builtins, imports) <- resolveImports defaultArguments tokens
    return $ run (tokenToAst builtins imports) tokens

spec :: Spec
spec = do
  functionSpec
  operatorSpec
  ifThenElseSpec
  whileSpec
  structureSpec
  getSpec
  isSpec
  isTypeSpec

isSpec :: SpecWith ()
isSpec = describe "is functions" $ do
  describe "isOp" $ do
    it "returns True for an Operator AST node" $ do
      let opNode = Operator "**" 8 IntType (IntType, "a") (IntType, "b") []
      isOp opNode `shouldBe` True

    it "returns False for a Structure AST node" $ do
      let structNode = Structure "test" [("field", IntType)]
      isOp structNode `shouldBe` False

  describe "Edge cases for expressions" $ do
    it "should handle empty function body gracefully" $ do
      let input = "function empty() -> void { }"
      result <- pAst input
      result `shouldSatisfy` isLeft

    it "should parse nested structures correctly" $ do
      let input = "struct nner { value: int } struct outer { onner: nner }"
      ast <- pAst input
      ast ==> [ Structure { structName = "nner", structMember = [("value", IntType)] }
              , Structure { structName = "outer", structMember = [("onner", StructType "nner")] } ]

    it "should fail with incompatible types in assignments" $ do
      let input = "function main() -> void { x: int = true; }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "should correctly parse recursive function calls" $ do
      let input = "function factorial(n: int) -> int { if n == 0 then { return 1; } else { return n * factorial(n - 1); } }"
      ast <- pAst input
      ast ==> [ Function "factorial" [(IntType, "n")] IntType
          [ IfThenElse (FunctionCall "==" [VariableCall "n", Lit (IntLit 0)])
                      [Return (Lit (IntLit 1))]
                      [Return (FunctionCall "*" [VariableCall "n", FunctionCall "factorial" [FunctionCall "-" [VariableCall "n", Lit (IntLit 1)]]])]
          ]
        ]

    -- it "should handle arrays of complex types" $ do
    --   let input = "function test() -> void { arr: arr[struct person] = [{name: \"Marius\", age: 25}, {name: \"Alex\", age: 30}]; }"
    --   ast <- pAst input
    --   ast ==> [Function
    --     "test" [] VoidType
    --     [ Variable { varMeta = (ArrType (StructType "person"), "arr"),
    --                 varValue = Lit (ArrLit (StructType "person")
    --                                 [ Lit (StructLit "person" [("name", Lit (StringLit "Marius")), ("age", Lit (IntLit 25))]),
    --                                   Lit (StructLit "person" [("name", Lit (StringLit "Alex")), ("age", Lit (IntLit 30))])]) }
    --     ]]

isTypeSpec :: SpecWith ()
isTypeSpec = describe "isType function" $ do
  describe "matching types" $ do
    it "returns True for an IntLit with IntType" $ do
      isType IntType (IntLit 42) `shouldBe` True

    it "returns True for a FloatLit with FloatType" $ do
      isType FloatType (FloatLit 1.5) `shouldBe` True

    it "returns True for a CharLit with CharType" $ do
      isType CharType (CharLit 'c') `shouldBe` True

    it "returns True for a BoolLit with BoolType" $ do
      isType BoolType (BoolLit True) `shouldBe` True

    it "returns True for a StringLit with StrType" $ do
      isType StrType (StringLit "hello") `shouldBe` True

    -- it "returns True for an ArrLitPre with an appropriate type" $ do
    --   let tokens = [[Token "1"], [Token "2"]]
    --   isType (ArrayType IntType) (ArrLitPre IntType tokens) `shouldBe` True

    -- it "returns True for an ArrLit with an appropriate type" $ do
    --   let elements = [SubExpression (IntLit 1), SubExpression (IntLit 2)]
    --   isType (ArrType IntType) (ArrLit IntType elements) `shouldBe` True

    -- it "returns True for a StructLitPre with matching fields" $ do
    --   let fields = [("name", [Token "\"marius\""]), ("age", [Token "19"])]
    --   isType (StructType "Person") (StructLitPre "Person" fields) `shouldBe` True

    -- it "returns True for a StructLit with matching fields" $ do
    --   let fields = [("name", SubExpression (StringLit "marius")), ("age", SubExpression (IntLit 19))]
    --   isType (StructType "Person") (StructLit "Person" fields) `shouldBe` True

    it "returns True for NullLit with NullType" $ do
      isType NullType NullLit `shouldBe` True

    it "returns True for a ClosureLit with matching parameters and return type" $ do
      isType (ClosureType [IntType] BoolType) (ClosureLit "func" [IntType] BoolType) `shouldBe` True

  describe "non-matching types" $ do
    it "returns False for an IntLit with FloatType" $ do
      isType FloatType (IntLit 42) `shouldBe` False

    it "returns False for a FloatLit with IntType" $ do
      isType IntType (FloatLit 1.5) `shouldBe` False

    it "returns False for a CharLit with StringType" $ do
      isType StrType (CharLit 'c') `shouldBe` False

    it "returns False for a BoolLit with IntType" $ do
      isType IntType (BoolLit True) `shouldBe` False

    it "returns False for NullLit with non-null type" $ do
      isType IntType NullLit `shouldBe` False

    it "returns False for a ClosureLit with mismatched return type" $ do
      isType (ClosureType [IntType] StrType) (ClosureLit "func" [IntType] BoolType) `shouldBe` False

getSpec :: SpecWith ()
getSpec = describe "get functions" $ do

  describe "getName" $ do
    it "returns the name of a Structure AST node" $ do
      getName (Structure "myStruct" []) `shouldBe` "myStruct"
    it "returns the name of a Function AST node" $ do
      getName (Function "myFunction" [] VoidType []) `shouldBe` "myFunction"

  describe "getNames" $ do
    it "extracts all names from a context with mixed AST nodes" $ do
      let ctx = [
            Structure "struct1" [],
            Function "fn1" [] VoidType [],
            Operator "op1" 0 IntType (IntType, "a") (IntType, "b") []
            ]
      getNames ctx `shouldBe` ["struct1", "fn1", "op1"]

  describe "getLitType" $ do
    it "returns the correct type for an IntLit" $ do
      getLitType (IntLit 42) `shouldBe` IntType

    it "returns the correct type for a StructLit" $ do
      getLitType (StructLit "myStruct" []) `shouldBe` StructType "myStruct"

    it "returns the correct type for a CharLit" $ do
      getLitType (CharLit 'c') `shouldBe` CharType

    it "returns the correct type for a BoolLit" $ do
      getLitType (BoolLit True) `shouldBe` BoolType

    it "returns the correct type for a FloatLit" $ do
      getLitType (FloatLit 1.5) `shouldBe` FloatType

    it "returns the correct type for a StringLit" $ do
      getLitType (StringLit "yo") `shouldBe` StrType

    it "returns the correct type for an ArrLitPre" $ do
      getLitType (ArrLitPre IntType [[Lit (IntLit 1), Lit (IntLit 2)]]) `shouldBe` ArrType IntType

    it "returns the correct type for an ArrLit" $ do
      getLitType (ArrLit IntType [Lit (IntLit 1), Lit (IntLit 2)]) `shouldBe` ArrType IntType

    it "returns the correct type for a StructLitPre" $ do
      getLitType (StructLitPre "myStruct" [("name", [Lit (StringLit "marius")]), ("age", [Lit (IntLit 19)])]) `shouldBe` StructType "myStruct"

    it "returns the correct type for a ClosureLit" $ do
      getLitType (ClosureLit "plus" [IntType, IntType] IntType) `shouldBe` ClosureType [IntType, IntType] IntType

    it "returns the correct type for a NullLit" $ do
      getLitType NullLit `shouldBe` NullType



structureSpec :: SpecWith ()
structureSpec = describe "structure parsing" $ do
  describe "valid cases" $ do
    it "parses a valid structure with multiple members" $ do
      let input = "struct elem { data: any, next: null | elem }"
      ast <- pAst input
      ast ==>
        [ Structure "elem"
            [ ("data", AnyType)
            , ("next", ConstraintType Nothing [NullType, StructType "elem"])
            ]
        ]
    it "parses a valid structure with a single member" $ do
      let input = "struct node { value: int }"
      ast <- pAst input
      ast ==>
        [ Structure "node"
            [ ("value", IntType)
            ]
        ]

  describe "invalid cases" $ do
    it "fails on missing semicolon between members" $ do
      let input = "struct elem { data: any next: null | elem; }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "fails on missing braces in structure definition" $ do
      let input = "struct elem data: any, next: null | elem;"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "fails on invalid type for a member" $ do
      let input = "struct elem { data: invalid_type; }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

  describe "isStruct" $ do
    it "returns True for a Structure AST node" $ do
      isStruct (Structure "test" [("field", IntType)]) `shouldBe` True
    it "returns False for a Function AST node" $ do
      isStruct (Function "testFn" [] VoidType []) `shouldBe` False

ifThenElseSpec :: SpecWith ()
ifThenElseSpec = describe "if-then-else" $ do
  describe "valid cases" $ do
    it "parses a valid if-then-else statement" $ do
      let input = "function main() -> void { x: int = 0; y: int = 0; if x < 10 then { y = y + 1; } else { y = 0; } }"
      ast <- pAst input
      ast ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            , Variable (IntType, "y") (Lit (IntLit 0))
            , IfThenElse (FunctionCall "<" [VariableCall "x", Lit (IntLit 10)])
                        [Variable (IntType, "y") (FunctionCall "+" [VariableCall "y", Lit (IntLit 1)])]
                        [Variable (IntType, "y") (Lit (IntLit 0))]
            ]
        ]
    it "parses a valid if-then statement without else" $ do
      let input = "function main() -> void { x: int = 0; y: int = 0; if x == 0 then { y = 1; } }"
      ast <- pAst input
      ast ==>
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
      let input = "function main() -> void { x: int = 0; y: int = 0; if x == 0 { y = 1; } else { y = 0; } }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "fails not boolean condition" $ do
      let input = "function main() -> void { x: int = 0; y: int = 0; if x + 0 then { y = 1; } else { y = 0; } }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "fails on missing braces in if-then-else" $ do
      let input = "function main()  -> void { x: int = 0; y: int = 0; if x == 0 then y = 1; else y = 0; }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft


whileSpec :: SpecWith ()
whileSpec = describe "while loop" $ do
  describe "valid cases" $ do
    it "parses a valid while loop with one statement" $ do
      let input = "function main () -> void { x: int = 0; while x < 10 then { x = x + 1; } }"
      ast <- pAst input
      ast ==>
        [ Function "main" [] VoidType
            [ Variable (IntType, "x") (Lit (IntLit 0))
            ,  While (FunctionCall "<" [VariableCall "x", Lit (IntLit 10)])
                    [Variable (IntType, "x") (FunctionCall "+" [VariableCall "x", Lit (IntLit 1)])]
            ]
        ]
    it "parses a valid while loop with multiple statements" $ do
      let input = "function main () -> void { x: int = 0; y: int = 0; while x < 10 then { x = x + 1; y = y + 2; } }"
      ast <- pAst input
      ast ==>
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
      let input = "function main () -> void { x: int = 0; while x < 10 then x = x + 1; }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft

    it "fails on empty body for while loop" $ do
      let input = "function main () -> void { x: int = 0; while x < 10 then {} }"
      ast <- pAst input
      ast `shouldSatisfy` isLeft


op0 :: String
op0 = [r|
operator **(a: int, b: int) -> int {
    return 0;
}
|]

opRec :: String
opRec = [r|
operator ** precedence 8 (a: int, b: int) -> int {
  return a ** (b - 1);
}
|]

operatorSpec :: SpecWith ()
operatorSpec = describe "operator" $ do
    describe "creation" $ do
      it "basic" $ do
        ast <- pAst op0
        ast ==> [Operator {opName = "**", opPrecedence = 0, opRetType = IntType, opArgLeft = (IntType,"a"), opArgRight = (IntType,"b"), opBody = [Return {retValue = Lit (IntLit 0)}]}]
      it "recursive" $ do
        ast <- pAst opRec
        ast ==> [Operator {opName = "**", opPrecedence = 8, opRetType = IntType, opArgLeft = (IntType,"a"), opArgRight = (IntType,"b"), opBody = [Return {retValue = FunctionCall {fnCallName = "**", fnCallArgs = [VariableCall {varCallName = "a"},FunctionCall {fnCallName = "-", fnCallArgs = [VariableCall {varCallName = "b"},Lit $ IntLit 1]}]}}]}]

fnNoParam :: String
fnNoParam = [r|
function zero() -> int {
    return 0;
}
|]

fnParam :: String
fnParam = [r|
function suc(x: int) -> int {
    return x + 1;
}
|]

functionSpec :: SpecWith ()
functionSpec = describe "function" $ do
    describe "creation" $ do
      it "no param" $ do
        ast <- pAst fnNoParam
        ast ==> [Function {fnName = "zero", fnArgs = [], fnRetType = IntType, fnBody = [Return {retValue = Lit (IntLit 0)}]}]
      it "one param" $ do
        ast <- pAst fnParam
        ast ==> [Function {fnName = "suc", fnArgs = [(IntType,"x")], fnRetType = IntType, fnBody = [Return {retValue = FunctionCall {fnCallName = "+", fnCallArgs = [VariableCall {varCallName = "x"},Lit (IntLit 1)]}}]}]
