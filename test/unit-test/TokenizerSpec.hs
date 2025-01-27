{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module contains unit tests for the Tokenizer component.
-}

module TokenizerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, Expectation, SpecWith, expectationFailure)
import Test.Hspec.Runner()

import Data.Either (isLeft)

import Parser.Tokenizer
import Parser.Token (MyToken (..), Type (..), Literal(..), Identifier(..))
import Control.Applicative ()
import Text.Megaparsec (parse, errorBundlePretty)
import Text.Megaparsec.Char ()
import Data.Void ()
import Control.Monad ()
import Utils.Lib
import qualified Data.List as List

(==>) :: (Show a, Eq a, Show b, Eq b) => Either a (pos, b) -> b -> Expectation
(==>) got expected = (snd <$> got) `shouldBe` Right expected

(==>>) :: (Show a, Eq a, Show b, Eq b) => Either a b -> b -> Expectation
(==>>) got expected = got `shouldBe` Right expected

(===) :: (Show a, Show b) => Either a b -> (Either a b -> Bool) -> Expectation
(===) got satisfy = got `shouldSatisfy` satisfy

spec :: Spec
spec = do
  commentSpec
  macroSpec
  tokenizerTypeSpec
  tokenizerKeywordSpec
  tokenizerSymbolSpec
  tokenizerLiteralSpec
  tokenizerIdentifierSpec
  tokenizerAdvancedSpec
  tokenizerEmptyCasesSpec
  tokenizerPos
  tokenizerAdditionalSpec

tokenizerIdentifierSpec :: SpecWith ()
tokenizerIdentifierSpec = describe "tokenize identifiers" $ do
  it "fibonacci" $
    run tokenize "fibonacci" ==> [Identifier $ TextId "fibonacci"]
  it "add1" $
    run tokenize "add1" ==> [Identifier $ TextId "add1"]
  it "str_cmp" $
    run tokenize "str_cmp" ==> [Identifier $ TextId "str_cmp"]
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
    run tokenize "\"yo\"" ==> [Literal $ StringLit "yo"]
  it "\"yo" $
    run tokenize "\"yo" === isLeft
  it "person { name = \"marius\", age = 1 }" $
    run tokenize "person { name = \"marius\", age = 1 }" ==> [Literal $ StructLitPre "person" [("name", [Literal $ StringLit "marius"]), ("age", [Literal $ IntLit 1])]]
  it "null {}" $
    run tokenize "null {}" ==> [Literal $ StructLitPre "null" []]
  it "int [1, 2]" $
    run tokenize "int [1, 2]" ==> [Literal $ ArrLitPre IntType [[Literal $ IntLit 1], [Literal $ IntLit 2]]]

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
  it "->" $
    run tokenize "->" ==> [Arrow]
  it ";" $
    run tokenize ";" ==> [SemiColon]
  it "," $
    run tokenize "," ==> [Comma]
  it "|" $
    run tokenize "|" ==> [Pipe]

tokenizerKeywordSpec :: SpecWith ()
tokenizerKeywordSpec = describe "tokenize keywords" $ do
  it "function" $
    run tokenize "function" ==> [FunctionKw]
  it "operator" $
    run tokenize "operator" ==> [OperatorKw]
  it "precedence" $
    run tokenize "precedence" ==> [PrecedenceKw]
  it "import std" $
    run tokenize "import std" ==> [ImportKw "std"]
  it "if" $
    run tokenize "if" ==> [IfKw]
  it "else" $
    run tokenize "else" ==> [ElseKw]
  it "return" $
    run tokenize "return" ==> [ReturnKw]
  it "atom" $
    run tokenize "atom" ==> [AtomKw]

tokenizerTypeSpec :: SpecWith ()
tokenizerTypeSpec = describe "tokenize types" $ do
  it "any" $
    run tokenize "any" ==> [Type AnyType]
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
  it "struct" $
    run tokenize "struct person" ==> [Type $ StructType "person"]
  it "type" $
    run tokenize "type number" ==> [Type $ ConstraintType (Just "number") []]

macroSpec :: SpecWith ()
macroSpec = describe "macro" $ do
  it "macro FAILURE = 84" $
    run macro "macro FAILURE 84\nreturn FAILURE;" ==>> "return 84;"
  it "macro FAILURE = 84 and eof" $
    run macro "macro FAILURE 84" ==>> ""
  it "macro FAILURE = 84 and skip string" $
    run macro "macro FAILURE 84\nreturn FAILURE; return \"string\"" ==>> "return 84; return \"string\""

commentSpec :: SpecWith ()
commentSpec = describe "comment" $ do
  it "no comment" $
    run comment "x = 5\ny = 6" ==>> "x = 5\ny = 6"
  it "unfinished string" $
    run comment "x = 5\"y = 6" === isLeft
  it "single line \\n" $
    run comment "x = 5// variable x\ny = 6" ==>> "x = 5             \ny = 6"
  it "single line eof" $
    run comment "x = 5// variable x" ==>> "x = 5             "
  it "single line and skip string" $
    run comment "x = \"lol\"// variable x" ==>> "x = \"lol\"             "
  it "multi line on single line" $
    run comment "x = 5/* variable x*/\ny = 6" ==>> "x = 5               \ny = 6"
  it "multi line on two lines" $
    run comment "x = 5/*\ncomment*/variable x" ==>> "x = 5  \n         variable x"
  it "multi line without closing part" $
    run comment "x = 5/*\ncommentvariable x" === isLeft
  it "multi line and skip string" $
    run comment "x = \"lol\"/* variable x*/\ny = 6" ==>> "x = \"lol\"               \ny = 6"

tokenizerAdvancedSpec :: SpecWith ()
tokenizerAdvancedSpec = describe "tokenize advanced / nested" $ do
  it "NULL" $
    run tokenize "NULL" ==> [Literal NullLit]
  it "struct with nested parentheses" $
    run tokenize "person { data = (1), other = ((2)) }" ==>
      [Literal $ StructLitPre "person"
        [ ("data",[ParenOpen, Literal (IntLit 1), ParenClose])
        , ("other",[ParenOpen, ParenOpen, Literal (IntLit 2), ParenClose, ParenClose])
        ]
      ]

  it "person { nested = { foo = 1 }, x = 2 }" $
    run tokenize "person { nested = { foo = 1 }, x = 2 }" ==>
      [Literal $ StructLitPre "person"
        [ ("nested",
          [ CurlyOpen
          , Identifier (TextId "foo"), Assign, Literal (IntLit 1)
          , CurlyClose
          ])
        , ("x",[Literal (IntLit 2)])
        ]
      ]

  it "int [(1)] -> triggers ParenOpen, ParenClose in array" $
    run tokenize "int [(1)]" ==> 
      [ Literal $ ArrLitPre IntType
          [ [ParenOpen, Literal (IntLit 1), ParenClose] ] 
      ]
  it "int [{y = 5}]" $
    run tokenize "int [{y = 5}]" ==>
      [ Literal $ ArrLitPre IntType
          [ [CurlyOpen, Identifier (TextId "y"), Assign, Literal (IntLit 5), CurlyClose] ]
      ]

tokenizerEmptyCasesSpec :: SpecWith ()
tokenizerEmptyCasesSpec = describe "tokenize empty cases" $ do
  it "int [] -> empty array" $
    run tokenize "int []" ==> [Literal $ ArrLitPre IntType []]
  it "type myConstraint -> ConstraintType (Just \"myConstraint\") []" $
    run tokenize "type myConstraint" ==> [Type $ ConstraintType (Just "myConstraint") []]
  it "tokenize type constraint with non-empty identifier" $
    run tokenize "type MyConstraint" ==> [Type $ ConstraintType (Just "MyConstraint") []]  

tokenizerPos :: SpecWith ()
tokenizerPos = describe "tokenize positions" $ do
    it "les positions du premier token sont (1,1)" $ do
      let input = "int"
          positions = fst <$> parse tokenize "" input
      positions `shouldBe` Right [(1, 1)]

tokenizerAdditionalSpec :: SpecWith ()
tokenizerAdditionalSpec = describe "tokenize additional patterns" $ do
  it "tokenizes assignment addition (+=)" $
    run tokenize "+=" ==> [AssignAdd]
  it "tokenizes assignment subtraction (-=)" $
    run tokenize "-=" ==> [AssignSub]
  it "tokenizes assignment multiplication (*=)" $
    run tokenize "*=" ==> [AssignMul]
  it "tokenizes assignment division (/=)" $
    run tokenize "/=" ==> [AssignDiv]
  it "tokenizes backslash (\\)" $
    run tokenize "\\" ==> [BackSlash]
  it "tokenizes a known symbol (for example ':' expected in tokens)" $
    run tokenize ":" ==> [Colon]
  it "parses an empty array using ListLitPre (when no type is given)" $
    run tokenize "[]" ==> [Literal $ ListLitPre []]
  it "parses an array with elements triggering the pattern 'return $ ListLitPre args'" $
    run tokenize "[1, 2, 3]" ==> [Literal $ ListLitPre [[Literal (IntLit 1)], [Literal (IntLit 2)], [Literal (IntLit 3)]]]
  it "triggers the generic pattern: (:) <$> oneTok <*> oneArg inFunction inArray inStruct" $
    run tokenize "[ (42) ]" ==> [Literal $ ListLitPre [[ParenOpen, Literal (IntLit 42), ParenClose]]]
  it "triggers the pattern for increasing the inArray counter: (:) <$> oneTok <*> oneArg inFunction (inArray + 1) inStruct" $
    run tokenize "[ 42 ]" ==> [Literal $ ListLitPre [[Literal (IntLit 42)]]]
  it "triggers the pattern for decreasing the inArray counter: (:) <$> oneTok <*> oneArg inFunction (inArray - 1) inStruct" $
    run tokenize "[ 42 ]" ==> [Literal $ ListLitPre [[Literal (IntLit 42)]]]
  it "returns a parse error with message indicating 'a known symbol' for an unknown token" $ 
    case parse tokenize "" "?" of
      Left err -> errorBundlePretty err `shouldSatisfy` ("a known symbol" `List.isInfixOf`)
      Right _  -> expectationFailure "Expected a parse error but got a successful parse"
