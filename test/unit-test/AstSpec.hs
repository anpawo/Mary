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
import Text.Megaparsec ()
import Text.Megaparsec.Error (ParseErrorBundle(..))
import Data.Either (isLeft)
import Data.Void (Void)

import Parser.Tokenizer
import Parser.Token
import Ast.Ast
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
  Right tokens -> do
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

isSpec :: SpecWith ()
isSpec = describe "is functions" $ do
  describe "isOp" $ do
    it "returns True for an Operator AST node" $ do
      let opNode = Operator "**" 8 IntType (IntType, "a") (IntType, "b") []
      isOp opNode `shouldBe` True

    it "returns False for a Structure AST node" $ do
      let structNode = Structure "test" [("field", IntType)]
      isOp structNode `shouldBe` False

  describe "types" $ do
    let ctx = []
        tokenizeInput input =
          case run tokenize input of
            Left _ -> Nothing
            Right tokens -> Just tokens
        parseType input = tokenizeInput input >>= \tokens ->
          case run (types ctx False False) tokens of
            Left _ -> Nothing
            Right t -> Just t

    it "parses a valid float type" $ do
      parseType "float" `shouldBe` Just FloatType

    it "parses a valid int type" $ do
      parseType "int" `shouldBe` Just IntType

    it "parses a valid char type" $ do
      parseType "char" `shouldBe` Just CharType

    it "fails on an invalid type" $ do
      parseType "invalidType" `shouldBe` Nothing

    it "parses a valid void type when allowed" $ do
      let parseVoidType input = tokenizeInput input >>= \tokens ->
            case run (types ctx True False) tokens of
              Left _ -> Nothing
              Right t -> Just t
      parseVoidType "void" `shouldBe` Just VoidType

    it "fails on void type when not allowed" $ do
      parseType "void" `shouldBe` Nothing

  describe "isType" $ do
    it "returns True if a literal matches its type" $ do
      isType IntType (IntLit 42) `shouldBe` True

    it "returns False if a literal does not match its type" $ do
      isType FloatType (IntLit 42) `shouldBe` False

  describe "notTaken" $ do
      let tokenizeInput input =
            case run tokenize input of
              Left _ -> Nothing
              Right tokens -> Just tokens

      let notTakenTest takenNames newName = do
            let builtins = map (\name -> Function { fnName = name, fnArgs = [], fnRetType = VoidType, fnBody = [] }) takenNames
            tokens <- maybe (fail "Tokenization failed") return (tokenizeInput newName)
            case run (tokenToAst builtins []) tokens of
              Left _ -> return Nothing
              Right context ->
                  if any (\case Function { fnName = n } -> n == newName; _ -> False) context
                  then return Nothing
                  else return (Just newName)

      it "allows a name that is not taken" $ do
        result <- notTakenTest ["name1", "name2"] "name3"
        result `shouldBe` Just "name3"

      it "fails for a name that is already taken" $ do
        result <- notTakenTest ["name1", "name2"] "name1"
        result `shouldBe` Nothing

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
    it "returns the correct type for a literal" $ do
      getLitType (IntLit 42) `shouldBe` IntType
    it "returns the correct type for a StructLit" $ do
      getLitType (StructLit "myStruct" []) `shouldBe` StructType "myStruct"

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
