{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module AstSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, Expectation, shouldSatisfy, SpecWith)
import Test.Hspec.Runner ()
import Text.RawString.QQ
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (ParseErrorBundle(..))
import Data.Either (isLeft)
import Data.Void (Void)

import Parser.Tokenizer
import Parser.Token
import Ast.Ast
import Ast.Error
import Ast.TreeBuilder ()
import Ast.DeclarationParser ()
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
  errorSpec
--   treeBuilderSpec

-- treeBuilderSpec :: SpecWith ()
-- treeBuilderSpec = describe "treeBuilder functions" $ do
--   describe "TreeBuilder.fixOp" $ do
--     it "handles an empty list" $ do
--       let result = runParser (subexpression ctx locVar endSubexpr) "" []
--       result `shouldBe` Right []

--     it "processes GOp followed by GVar" $ do
--       let group = [GOp 0 "." [], GVar 1 "x", GVar 2 "y"]
--       let expected = [GOp 0 "." [], GLit 1 (StringLit "x"), GVar 2 "y"]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       result `shouldBe` Right expected

--     it "fails on invalid field after GOp '.'" $ do
--       let group = [GOp 0 "." [], GVar 1 "x", GOp 2 "+" []]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       case result of
--         Left err -> err `shouldContain` "Expected field after '.'"
--         Right _ -> expectationFailure "Expected error"

--     it "processes a list of GVar and GOp" $ do
--       let group = [GVar 0 "a", GOp 1 "+" [], GVar 2 "b"]
--       let expected = [GVar 0 "a", GOp 1 "+" [], GVar 2 "b"]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       result `shouldBe` Right expected

--     it "handles a list with GFn" $ do
--       let group = [GFn 0 "myFunc" [GVar 1 "a"], GVar 2 "b"]
--       let expected = [GFn 0 "myFunc" [GVar 1 "a"], GVar 2 "b"]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       result `shouldBe` Right expected

--     it "processes a list with GGr" $ do
--       let group = [GGr 0 [GVar 1 "a"]]
--       let expected = [GGr 0 [GVar 1 "a"]]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       result `shouldBe` Right expected

--     it "handles GLit in the list" $ do
--       let group = [GLit 0 (IntLit 42), GOp 1 "+" [], GLit 2 (StringLit "test")]
--       let expected = [GLit 0 (IntLit 42), GOp 1 "+" [], GLit 2 (StringLit "test")]
--       let result = runParser (subexpression ctx locVar endSubexpr) "" group
--       result `shouldBe` Right expected

errorSpec :: SpecWith ()
errorSpec = describe "error functions" $ do
  describe "Error Messages" $ do

    it "errCondNotBool formats correctly" $ do
      errCondNotBool 42 `shouldBe` ":42expected a \ESC[94mboolean\ESC[0m condition."

    it "errNameTaken formats correctly" $ do
      errNameTaken "x" `shouldBe` "name '\ESC[94mx\ESC[0m' already taken."

    it "errExpectedType formats correctly with void allowed" $ do
      errExpectedType True `shouldBe` "expected a \ESC[94mtype\ESC[0m (including void)."

    it "errExpectedType formats correctly without void allowed" $ do
      errExpectedType False `shouldBe` "expected a \ESC[94mtype\ESC[0m (excluding void)."

    it "errStartBody formats correctly" $ do
      errStartBody `shouldBe` "expected a '\ESC[94m{\ESC[0m' representing the start of the body of the function."

    it "errEndBody formats correctly" $ do
      errEndBody `shouldBe` "expected a '\ESC[94m}\ESC[0m' representing the end of the body of the function."

    it "errVoidRet formats correctly" $ do
      errVoidRet `shouldBe` "\ESC[94mvoid\ESC[0m function should not \ESC[94mreturn\ESC[0m."

    it "errEndSubexpr formats correctly" $ do
      errEndSubexpr `shouldBe` "expected end of expression '\ESC[94m;\ESC[0m'."

    it "errEndExpr formats correctly" $ do
      errEndExpr `shouldBe` "expected the end of the expression with '\ESC[94m;\ESC[0m' or '\ESC[94m)\ESC[0m'."

    it "errEndParen formats correctly" $ do
      errEndParen `shouldBe` "expected the end of the expression with a '\ESC[94m)\ESC[0m'."

    it "errEmptyParen formats correctly" $ do
      errEmptyParen `shouldBe` ":2expected an expression inside the \ESC[94mparenthesis\ESC[0m."

    it "errEmptyExpr formats correctly" $ do
      errEmptyExpr `shouldBe` "expected an expression."

    it "errExpectedField formats correctly" $ do
      errExpectedField `shouldBe` "expected a structure field."

    it "errExpectedStartField formats correctly" $ do
      errExpectedStartField `shouldBe` "expected a '\ESC[94m{\ESC[0m' followed by the fields defition."

    it "errExpectedFieldName formats correctly" $ do
      errExpectedFieldName `shouldBe` "expected a field name."

    it "errMissingRetT formats correctly" $ do
      errMissingRetT `shouldBe` "expected a '\ESC[94m->\ESC[0m' followed by the return type of the function."

    it "errMisingNameFn formats correctly" $ do
      errMisingNameFn `shouldBe` "expected a name made of letters."

    it "errMisingEqual formats correctly" $ do
      errMisingEqual `shouldBe` "expected a '\ESC[94m=\ESC[0m' followed by the types."

    it "errMisingNameOp formats correctly" $ do
      errMisingNameOp `shouldBe` "expected a name made of symbols."

    it "errMisingPrecOp formats correctly" $ do
      errMisingPrecOp `shouldBe` "expected the precedence as an '\ESC[94minteger\ESC[0m'."

    it "errExpectedCommaOrCurly formats correctly" $ do
      errExpectedCommaOrCurly `shouldBe` "expected a '\ESC[94m,\ESC[0m' to separate the fields or a '\ESC[94m}\ESC[0m' to end their definition."

    it "errExpectedCommaOrParen formats correctly" $ do
      errExpectedCommaOrParen `shouldBe` "expected a '\ESC[94m,\ESC[0m' to separate the arguments or a '\ESC[94m)\ESC[0m' to end their definition."

    it "errExpectedColon formats correctly" $ do
      errExpectedColon `shouldBe` "expected a '\ESC[94m:\ESC[0m' to separate the name of the argument from it's type."

    it "errExpectedArgName formats correctly" $ do
      errExpectedArgName `shouldBe` "expected the name of the argument made of letters."

    it "errExpectedArgs formats correctly" $ do
      errExpectedArgs `shouldBe` "expected a '\ESC[94m(\ESC[0m' followed by the arguments."

    it "errMisingBody formats correctly" $ do
      errMisingBody `shouldBe` "the body of a function cannot be empty."

    it "errSemiColon formats correctly" $ do
      errSemiColon `shouldBe` "expected a '\ESC[94m;\ESC[0m' at the end of the expression."

    it "errOpNotDefined formats correctly" $ do
      errOpNotDefined "+" `shouldBe` "the operator '\ESC[94m+\ESC[0m' is not defined."

    it "errMissingOperand formats correctly" $ do
      errMissingOperand "left" "+" `shouldBe` "missing the \ESC[94mleft\ESC[0m operand for the operator '\ESC[94m+\ESC[0m'."

    it "errInvalidExpr formats correctly" $ do
      errInvalidExpr 10 `shouldBe` ":10invalid expression."

    it "errOpArgs formats correctly" $ do
      errOpArgs 1 1 "+" `shouldBe` ":1operators must take 2 arguments. '\ESC[94m+\ESC[0m' has \ESC[94m1\ESC[0m argument."

    it "errRetType formats correctly" $ do
      errRetType "int" "string" `shouldBe` "invalid return type, expected '\ESC[94mint\ESC[0m' got '\ESC[94mstring\ESC[0m'."

    it "errAssignType formats correctly" $ do
      errAssignType "x" "int" "string" `shouldBe` "invalid type for the variable '\ESC[94mx\ESC[0m', expected '\ESC[94mint\ESC[0m' got '\ESC[94mstring\ESC[0m'."

    it "errTopLevelDef formats correctly" $ do
      errTopLevelDef `shouldBe` "top level declaration must be either \ESC[94mfunction\ESC[0m, \ESC[94moperator\ESC[0m, \ESC[94mstruct\ESC[0m or \ESC[94mtype\ESC[0m."

    it "errImpossibleCase formats correctly" $ do
      errImpossibleCase "this should not happen" `shouldBe` "Impossible case. (from \ESC[94mthis should not happen\ESC[0m)."

    it "errVariableNotBound formats correctly" $ do
      errVariableNotBound "x" `shouldBe` "variable '\ESC[94mx\ESC[0m' is not bound."

    it "errFunctionNotBound formats correctly" $ do
      errFunctionNotBound "f" `shouldBe` "function '\ESC[94mf\ESC[0m' doesn't exist."

    it "errStructureNotBound formats correctly" $ do
      errStructureNotBound "MyStruct" `shouldBe` "structure '\ESC[94mMyStruct\ESC[0m' doesn't exist."

    it "errStructureFieldNotBound formats correctly" $ do
      errStructureFieldNotBound "MyStruct" "field" `shouldBe` "structure '\ESC[94mMyStruct\ESC[0m' doesn't have the field '\ESC[94mfield\ESC[0m'."

    it "errConstraintNotBound formats correctly" $ do
      errConstraintNotBound "C" `shouldBe` "constraint '\ESC[94mC\ESC[0m' doesn't exist."

    it "errInvalidStructure formats correctly" $ do
      errInvalidStructure "MyStruct" [("field1", IntType), ("field2", BoolType)]
        `shouldBe` "invalid structure '\ESC[94mMyStruct\ESC[0m', expected:\n{\n    \ESC[94mfield1\ESC[0m = \ESC[94mint\ESC[0m,\n    \ESC[94mfield2\ESC[0m = \ESC[94mbool\ESC[0m\n}"

    it "errInvalidArray formats correctly" $ do
      errInvalidArray "int" `shouldBe` "invalid array of type '\ESC[94mint\ESC[0m'."

    it "errOperatorNotBound formats correctly" $ do
      errOperatorNotBound "+" `shouldBe` "operator '\ESC[94m+\ESC[0m' is not bound."

    it "errInvalidLitType formats correctly" $ do
      errInvalidLitType "int" "string" `shouldBe` "invalid literal type, expected \ESC[94mint\ESC[0m but got \ESC[94mstring\ESC[0m."

    it "errInvalidVarType formats correctly" $ do
      errInvalidVarType "x" "int" "string" `shouldBe` "invalid variable type for '\ESC[94mx\ESC[0m', expected '\ESC[94mint\ESC[0m' but got '\ESC[94mstring\ESC[0m'."

    it "errInvalidFnType formats correctly" $ do
      errInvalidFnType "f" "int -> int" "string -> int"
        `shouldBe` "invalid function type for '\ESC[94mf\ESC[0m', expected '\ESC[94mint -> int\ESC[0m' but got '\ESC[94mstring -> int\ESC[0m'."

    it "errInvalidOpType formats correctly" $ do
      errInvalidOpType "+" "int -> int -> int" "string -> string -> string"
        `shouldBe` "invalid operator type for '\ESC[94m+\ESC[0m', expected '\ESC[94mint -> int -> int\ESC[0m' but got '\ESC[94mstring -> string -> string\ESC[0m'."

    it "errInvalidNumberOfArgument formats correctly" $ do
      errInvalidNumberOfArgument "f" 2 3
        `shouldBe` "invalid number of arguments for the function '\ESC[94mf\ESC[0m', expected \ESC[94m2\ESC[0m but found \ESC[94m3\ESC[0m."

  describe "colorblindMode" $ do
    it "replaces red with yellow for colorblind mode" $ do
      colorblindMode "\ESC[91mError\ESC[0m" `shouldBe` "\ESC[93mError\ESC[0m"

    it "replaces blue with pink for colorblind mode" $ do
      colorblindMode "\ESC[94mInfo\ESC[0m" `shouldBe` "\ESC[95mInfo\ESC[0m"

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
    --   let tokens = [[Literal (IntLit 1), Literal (IntLit 2)]]
    --   isType (ArrType IntType) (ArrLitPre IntType tokens) `shouldBe` True

    it "returns True for an ArrLit with an appropriate type" $ do
      let elements = [Lit (IntLit 1), Lit (IntLit 2)]
      isType (ArrType IntType) (ArrLit IntType elements) `shouldBe` True

    -- it "returns True for a StructLitPre with matching fields" $ do
    --   let fields = [("name", [Literal (StringLit "\"marius\"")]), ("age", [Literal (IntLit 19)])]
    --   isType (StructType "Person") (StructLitPre "Person" fields) `shouldBe` True

    it "returns True for a StructLit with matching fields" $ do
      let fields = [("name", Lit (StringLit "marius")), ("age", Lit (IntLit 19))]
      isType (StructType "Person") (StructLit "Person" fields) `shouldBe` True

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
      getLitType (ArrLitPre IntType [[Literal (IntLit 1), Literal (IntLit 2)]]) `shouldBe` IntType

    it "returns the correct type for an ArrLit" $ do
      getLitType (ArrLit IntType [Lit (IntLit 1), Lit (IntLit 2)]) `shouldBe` IntType

    it "returns the correct type for a StructLitPre" $ do
      getLitType (StructLitPre "myStruct" [("name", [Literal (StringLit "marius")]), ("age", [Literal (IntLit 19)])]) `shouldBe` StructType "myStruct"

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
