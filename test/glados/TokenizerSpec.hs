{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module TokenizerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, Expectation)
import Test.Hspec.Runner (SpecWith)

import Data.Either (isLeft)

import Parser.Tokenizer
import Parser.Token
import Utils.Lib

(==>) :: (Show a, Eq a, Show b, Eq b) => Either a b -> b -> Expectation
(==>) got expected = got `shouldBe` Right expected

(===) :: (Show a, Show b) => Either a b -> (Either a b -> Bool) -> Expectation
(===) got satisfy = got `shouldSatisfy` satisfy

spec :: Spec
spec = do
  commentSpec
  macroSpec
  namespaceSpec
  tokenizerTypeSpec
  tokenizerKeywordSpec
  tokenizerSymbolSpec
  tokenizerLiteralSpec
  tokenizerIdentifierSpec
  tokenizerUtils

tokenizerUtils :: SpecWith ()
tokenizerUtils = describe "utils" $ do
  it "&>" $
    run (macro &> namespace) "macro FAILURE 84\nreturn FAILURE;import math\nmath.facto" ==> "return 84;_ZN4mathfacto"

tokenizerIdentifierSpec :: SpecWith ()
tokenizerIdentifierSpec = describe "tokenize identifiers" $ do
  it "fibonacci" $
    run tokenize "fibonacci" ==> [Identifier $ SymbolId "fibonacci"]
  it "add1" $
    run tokenize "add1" ==> [Identifier $ SymbolId "add1"]
  it "str_cmp" $
    run tokenize "str_cmp" ==> [Identifier $ SymbolId "str_cmp"]
  it "+" $
    run tokenize "+" ==> [Identifier $ OperatorId "+"]
  it "<*>" $
    run tokenize "<*>" ==> [Identifier $ OperatorId "<*>"]

tokenizerLiteralSpec :: SpecWith ()
tokenizerLiteralSpec = describe "tokenize literals" $ do
  it "'c'" $
    run tokenize "'c'" ==> [Literal $ CharLit 'c']
  it "'c" $
    run tokenize "'c" === isLeft
  it "true" $
    run tokenize "true" ==> [Literal $ BoolLit True]
  it "false" $
    run tokenize "false" ==> [Literal $ BoolLit False]
  it "4" $
    run tokenize "4" ==> [Literal $ IntLit 4]
  it "-4" $
    run tokenize "-4" ==> [Literal $ IntLit (-4)]
  it "-4.2" $
    run tokenize "-4.2" ==> [Literal $ FloatLit (-4.2)]
  it "4.2" $
    run tokenize "4.2" ==> [Literal $ FloatLit 4.2]
  it "\"yo\"" $
    run tokenize "\"yo\"" ==> [Literal $ StrLit "yo"]
  it "\"yo" $
    run tokenize "\"yo" === isLeft

tokenizerSymbolSpec :: SpecWith ()
tokenizerSymbolSpec = describe "tokenize symbols" $ do
  it "{" $
    run tokenize "{" ==> [CurlyOpen]
  it "}" $
    run tokenize "}" ==> [CurlyClose]
  it "(" $
    run tokenize "(" ==> [ParenOpen]
  it ")" $
    run tokenize ")" ==> [ParenClose]
  it "[" $
    run tokenize "[" ==> [BracketOpen]
  it "]" $
    run tokenize "]" ==> [BracketClose]
  it "=" $
    run tokenize "=" ==> [Assign]
  it "->" $
    run tokenize "->" ==> [Arrow]
  it "." $
    run tokenize "." ==> [Scope]
  it ";" $
    run tokenize ";" ==> [SemiColon]
  it "," $
    run tokenize "," ==> [Comma]

tokenizerKeywordSpec :: SpecWith ()
tokenizerKeywordSpec = describe "tokenize keywords" $ do
  it "function" $
    run tokenize "function" ==> [FunctionKw]
  it "operator" $
    run tokenize "operator" ==> [OperatorKw]
  it "precedence" $
    run tokenize "precedence" ==> [PrecedenceKw]
  it "struct" $
    run tokenize "struct" ==> [StructKw]
  it "is" $
    run tokenize "is" ==> [IsKw]  
  it "import" $
    run tokenize "import" ==> [ImportKw]
  it "as" $
    run tokenize "as" ==> [AsKw]
  it "at" $
    run tokenize "at" ==> [AtKw]
  it "if" $
    run tokenize "if" ==> [IfKw]
  it "else" $
    run tokenize "else" ==> [ElseKw]
  it "return" $
    run tokenize "return" ==> [ReturnKw]

tokenizerTypeSpec :: SpecWith ()
tokenizerTypeSpec = describe "tokenize types" $ do
  it "char" $
    run tokenize "char" ==> [Type CharType]
  it "void" $
    run tokenize "void" ==> [Type VoidType]
  it "bool" $
    run tokenize "bool" ==> [Type BoolType]
  it "int" $
    run tokenize "int" ==> [Type IntType]
  it "float" $
    run tokenize "float" ==> [Type FloatType]
  it "str" $
    run tokenize "str" ==> [Type StrType]
  it "arr" $
    run tokenize "arr[int]" ==> [Type $ ArrType IntType]

namespaceSpec :: SpecWith ()
namespaceSpec= describe "namespace" $ do
  it "import math" $
    run namespace "import math\nmath.facto" ==> "_ZN4mathfacto"
  it "import math and eof" $
    run namespace "import math" ==> ""
  it "import math and skip string" $
    run namespace "import math\nmath.facto \"string\"" ==> "_ZN4mathfacto \"string\""

macroSpec :: SpecWith ()
macroSpec = describe "macro" $ do
  it "macro FAILURE = 84" $
    run macro "macro FAILURE 84\nreturn FAILURE;" ==> "return 84;"
  it "macro FAILURE = 84 and eof" $
    run macro "macro FAILURE 84" ==> ""
  it "macro FAILURE = 84 and skip string" $
    run macro "macro FAILURE 84\nreturn FAILURE; return \"string\"" ==> "return 84; return \"string\""

commentSpec :: SpecWith ()
commentSpec = describe "comment" $ do
  it "no comment" $
    run comment "x = 5\ny = 6" ==> "x = 5\ny = 6"
  it "unfinished string" $
    run comment "x = 5\"y = 6" === isLeft
  it "single line \\n" $
    run comment "x = 5// variable x\ny = 6" ==> "x = 5\ny = 6"
  it "single line eof" $
    run comment "x = 5// variable x" ==> "x = 5"
  it "single line and skip string" $
    run comment "x = \"lol\"// variable x" ==> "x = \"lol\""
  it "multi line on single line" $
    run comment "x = 5/* variable x*/\ny = 6" ==> "x = 5\ny = 6"
  it "multi line on two lines" $
    run comment "x = 5/*\ncomment*/variable x" ==> "x = 5variable x"
  it "multi line without closing part" $
    run comment "x = 5/*\ncommentvariable x" === isLeft
  it "multi line and skip string" $
    run comment "x = \"lol\"/* variable x*/\ny = 6" ==> "x = \"lol\"\ny = 6"
