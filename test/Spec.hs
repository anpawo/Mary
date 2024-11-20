{-
-- EPITECH PROJECT, 2024
-- Spec.hs
-- File description:
-- glados
-}

import Test.HUnit
import Parser
import SExprParser

testParseChar :: Test
testParseChar = TestCase $ do
    assertEqual "parseChar 'a' in 'abc' want (a, bc)" (Right ('a', "bc")) (runParser (parseChar 'a') "abc")
    assertEqual "parseChar 'a' in 'bbc' want error a not found" (Left "a not found") (runParser (parseChar 'a') "bbc")

testParseNotThisChar :: Test
testParseNotThisChar = TestCase $ do
    assertEqual "parseNotThisChar 'a' in 'bbc' want (b, bc)" (Right ('b', "bc")) (runParser (parseNotThisChar 'a') "bbc")
    assertEqual "parseNotThisChar 'a' in 'abc' want error a found" (Left "a found") (runParser (parseNotThisChar 'a') "abc")

testParseNotTheseChars :: Test
testParseNotTheseChars = TestCase $ do
    assertEqual "parseNotTheseChars 'abc' in 'defg' want (d, efg)" (Right ('d', "efg")) (runParser (parseNotTheseChars "abc") "defg")
    assertEqual "parseNotTheseChars 'abc' in 'abc' want error abc found" (Left "abc found") (runParser (parseNotTheseChars "abc") "abc")

testParseAnyChar :: Test
testParseAnyChar = TestCase $ do
    assertEqual "parseAnyChar 'abc' in 'abc' want (a, bc)" (Right ('a', "bc")) (runParser (parseAnyChar "abc") "abc")
    assertEqual "parseAnyChar 'abc' in 'dcba' want error abc found" (Left "abc not found") (runParser (parseAnyChar "abc") "dcba")

testParseOr :: Test
testParseOr = TestCase $ do
    assertEqual "parseOr using parseChar 'a' and parseChar 'b' in 'abc' want (a, bc)" (Right ('b', "abc")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "babc")
    assertEqual "parseOr using parseChar 'a' and parseChar 'b' in 'dcba' want error" (Left "b not found") (runParser (parseOr (parseChar 'a') (parseChar 'b')) "dcba")

testParseAnd :: Test
testParseAnd = TestCase $ do
    assertEqual "parseAnd with matching 'a' and 'b'" (Right (('a', 'b'), "cd")) (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd")
    assertEqual "parseAnd with non-matching 'b' and 'a'" (Left "b not found") (runParser (parseAnd (parseChar 'b') (parseChar 'a')) "abcd")

testParseMany :: Test
testParseMany = TestCase $ do
    assertEqual "parseMany 'a' on 'aaa'" (Right ("aaa", "")) (runParser (parseMany (parseChar 'a')) "aaa")
    assertEqual "parseMany 'a' on 'abc'" (Right ("a", "bc")) (runParser (parseMany (parseChar 'a')) "abc")
    assertEqual "parseMany 'a' on empty" (Right ("", "")) (runParser (parseMany (parseChar 'a')) "")

testParseSome :: Test
testParseSome = TestCase $ do
    assertEqual "parseSome 'a' on 'aaa'" (Right ("aaa", "")) (runParser (parseSome (parseChar 'a')) "aaa")
    assertEqual "parseSome 'a' on 'abc'" (Right ("a", "bc")) (runParser (parseSome (parseChar 'a')) "abc")
    assertEqual "parseSome 'a' on empty" (Left "Fail") (runParser (parseSome (parseChar 'a')) "")

testParseUInt :: Test
testParseUInt = TestCase $ do
    assertEqual "parseUInt on '123'" (Right (123, "")) (runParser parseUInt "123")
    assertEqual "parseUInt on 'abc'" (Left "Fail") (runParser parseUInt "abc")

testParseInt :: Test
testParseInt = TestCase $ do
    assertEqual "parseInt on '123'" (Right (123, "")) (runParser parseInt "123")
    assertEqual "parseInt on '-123'" (Right (-123, "")) (runParser parseInt "-123")
    assertEqual "parseInt on 'abc'" (Left "Parse int failed") (runParser parseInt "abc")

testParseWord :: Test
testParseWord = TestCase $ do
    assertEqual "parseWord 'hello' on 'hello world'" (Right ("hello", " world")) (runParser (parseWord "hello") "hello world")
    assertEqual "parseWord 'hello' on 'hella world'" (Left "Word not found") (runParser (parseWord "hello") "hella world")

testParseString :: Test
testParseString = TestCase $ do
    assertEqual "parseString on '\"abc\"'" (Right ("abc", "")) (runParser parseString "\"abc\"")
    assertEqual "parseString on 'abc'" (Left "Fail") (runParser parseString "abc")

testParsePrefix :: Test
testParsePrefix = TestCase $ do
    assertEqual "parsePrefix on 'prefix' with 'prefixHello'" (Right ("prefix", "Hello")) (runParser (parsePrefix "prefix") "prefixHello")
    assertEqual "parsePrefix on 'pre' with 'prefixHello'" (Right ("pre", "fixHello")) (runParser (parsePrefix "pre") "prefixHello")
    assertEqual "parsePrefix on 'not' with 'prefixHello'" (Left "Fail") (runParser (parsePrefix "not") "prefixHello")

testParseSExprAtomInt  :: Test
testParseSExprAtomInt  = TestCase $ do
    assertEqual "parseSExprAtomInt on '123'" (Right (SExprAtomInt 123, "")) (runParser parseSExprAtomInt  "123")
    assertEqual "parseSExprAtomInt  on '        123'" (Right (SExprAtomInt 123, "")) (runParser parseSExprAtomInt  "        123")
    assertEqual "parseSExprAtomInt  on 'abc'" (Left "Fail") (runParser parseSExprAtomInt  "abc")
    assertEqual "parseSExprAtomInt  on '12abc'" (Left "Fail") (runParser parseSExprAtomInt  "12abc")

testParseSExprAtomString  :: Test
testParseSExprAtomString  = TestCase $ do
    assertEqual "parseSExprAtomString on '123'" (Right (SExprAtomString "123", "")) (runParser parseSExprAtomString  "123")
    assertEqual "parseSExprAtomString  on 'abc'" (Right (SExprAtomString "abc", "")) (runParser parseSExprAtomString  "abc")

testParseSExprList  :: Test
testParseSExprList  = TestCase $ do
    assertEqual "parseSExprList  on '(define x 5)'" (Right (SExprList [SExprAtomString "define", SExprAtomString "x", SExprAtomInt 5], "")) (runParser parseSExprList "(define x 5)")
    assertEqual "parseSExprList  on '(x 5 6 (* 2 abc))'" (Right (SExprList [SExprAtomString "x", SExprAtomInt 5, SExprAtomInt 6, SExprList [SExprAtomString "*", SExprAtomInt 2, SExprAtomString "abc"]], "")) (runParser parseSExprList "(x 5 6 (* 2 abc))")
    assertEqual "parseSExprList  on 'define x 5)'" (Left "( not found") (runParser parseSExprList  "define x 5)")
    assertEqual "parseSExprList  on '(define x 5'" (Left "Empty string") (runParser parseSExprList  "(define x 5") --Empty string apparait parce que la string est vide quand on cherche la parenthèse fermante

tests :: Test
tests = TestList [
    TestLabel "testParseChar" testParseChar,
    TestLabel "testParseNotThisChar" testParseNotThisChar,
    TestLabel "testParseNotTheseChars" testParseNotTheseChars,
    TestLabel "testParseAnyChar" testParseAnyChar,
    TestLabel "testParseOr" testParseOr,
    TestLabel "testParseAnd" testParseAnd,
    TestLabel "testParseMany" testParseMany,
    TestLabel "testParseSome" testParseSome,
    TestLabel "testParseUInt" testParseUInt,
    TestLabel "testParseInt" testParseInt,
    TestLabel "testParseWord" testParseWord,
    TestLabel "testParseString" testParseString,
    TestLabel "testParsePrefix" testParsePrefix,
    TestLabel "testParseSExprAtomInt" testParseSExprAtomInt,
    TestLabel "testParseSExprAtomString" testParseSExprAtomString,
    TestLabel "testParseSexprList" testParseSExprList
  ]

main :: IO ()
main = do
    counts <- runTestTT tests
    print counts