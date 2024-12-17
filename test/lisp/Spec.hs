{-
-- EPITECH PROJECT, 2024
-- Spec.hs
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

import Test.HUnit
import Parser
import SExprParser
import AST

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
    assertEqual "parseInt on 'abc'" (Left "Fail") (runParser parseInt "abc")

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

testFindDefine :: Test
testFindDefine = TestCase $ do
    let defs = [Define "x" (AstInt 10), Define "y" (AstBool True)]
    case findDefine defs "x" of
        Right (AstInt 10) -> return ()
        _ -> assertFailure "Find existing definition: Expected AstInt 10"
    case findDefine defs "z" of
        Left err | err == "Define with name 'z' not found" -> return ()
        _ -> assertFailure "Fail to find non-existent definition: Expected error 'Define with name 'z' not found'"

testSexprToAST :: Test
testSexprToAST = TestCase $ do
    case sexprToAST (SExprAtomInt 42) of
        Right (AstInt 42) -> return ()
        _ -> assertFailure "Convert SExprAtomInt 42: Expected AstInt 42"
    case sexprToAST (SExprAtomString "#t") of
        Right (AstBool True) -> return ()
        _ -> assertFailure "Convert SExprAtomString '#t': Expected AstBool True"
    case sexprToAST (SExprAtomString "#f") of
        Right (AstBool False) -> return ()
        _ -> assertFailure "Convert SExprAtomString '#f': Expected AstBool False"
    case sexprToAST (SExprList [SExprAtomString "+", SExprAtomInt 1, SExprAtomInt 2]) of
        Right (AstFunction (Function "+" [AstInt 1, AstInt 2])) -> return ()
        _ -> assertFailure "Convert simple function call (+ 1 2): Expected AstFunction '+' with arguments [AstInt 1, AstInt 2]"
    case sexprToAST (SExprList [SExprAtomString "if", SExprAtomString "#t", SExprAtomInt 1, SExprAtomInt 0]) of
        Right (AstCondition (Condition (AstBool True) (AstInt 1) (AstInt 0))) -> return ()
        _ -> assertFailure "Convert if condition: Expected AstCondition with correct branches"
    case sexprToAST (SExprList [SExprAtomString "define", SExprAtomString "x", SExprAtomInt 5]) of
        Right (AstDefine (Define "x" (AstInt 5))) -> return ()
        _ -> assertFailure "Convert define expression: Expected AstDefine with name 'x' and value AstInt 5"
    -- case sexprToAST (SExprAtomString "!") of
    --     Left err | err == "Unrecognized SExpr" -> return ()
    --     _ -> assertFailure "Error on unrecognized SExpr: Expected error 'Unrecognized SExpr'"
    case sexprToAST (SExprList [SExprAtomString "lambda", SExprList [SExprAtomString "x"],
                                SExprList [SExprAtomString "+", SExprAtomString "x", SExprAtomInt 1]]) of
        Right (AstLambda (Lambda [AstStr "x"] (AstFunction (Function "+" [AstStr "x", AstInt 1])))) -> return ()
        _ -> assertFailure "Convert lambda expression: Expected correct AstLambda representation"

testEvalAST :: Test
testEvalAST = TestCase $ do
    case evalAST [] (AstInt 42) of
        Right (env, AstInt val)
            | null env && val == 42 -> return ()
        _ -> assertFailure "Eval integer: Expected ([], AstInt 42)"
    case evalAST [] (AstCondition (Condition (AstBool True) (AstInt 1) (AstInt 0))) of
        Right (env, AstInt val)
            | null env && val == 1 -> return ()
        _ -> assertFailure "Eval condition (if #t 1 0): Expected ([], AstInt 1)"
    case evalAST [] (AstFunction (Function "+" [AstInt 1, AstInt 2])) of
        Right (env, AstInt val)
            | null env && val == 3 -> return ()
        _ -> assertFailure "Eval addition (+ 1 2): Expected ([], AstInt 3)"
    let defs = [Define "x" (AstInt 10)]
    case evalAST defs (AstStr "x") of
        Right (defs', AstInt val)
            | all (checkDefine defs) defs' && val == 10 -> return ()
        _ -> assertFailure "Eval variable 'x': Expected ([Define \"x\" (AstInt 10)], AstInt 10)"
    case evalAST [] (AstStr "y") of
        Left err
            | err == "Define with name 'y' not found" -> return ()
        _ -> assertFailure "Error for undefined variable 'y': Expected error 'Define with name 'y' not found'"

checkDefine :: [Define] -> Define -> Bool
checkDefine defs (Define name value) =
    any (\(Define n v) -> n == name && compareAst v value) defs

compareAst :: AST -> AST -> Bool
compareAst (AstInt x) (AstInt y) = x == y
compareAst (AstBool x) (AstBool y) = x == y
compareAst (AstStr x) (AstStr y) = x == y
compareAst (AstFunction (Function f1 args1)) (AstFunction (Function f2 args2)) =
    f1 == f2 && all (uncurry compareAst) (zip args1 args2)
compareAst (AstCondition (Condition c1 t1 e1)) (AstCondition (Condition c2 t2 e2)) =
    compareAst c1 c2 && compareAst t1 t2 && compareAst e1 e2
compareAst (AstDefine (Define name1 value1)) (AstDefine (Define name2 value2)) =
    name1 == name2 && compareAst value1 value2
compareAst _ _ = False

testCountFunctionArgs :: Test
testCountFunctionArgs = TestCase $ do
    let func = AstFunction (Function "f" [AstInt 1, AstInt 2])
    assertEqual "Count arguments of a function"
        2
        (countFunctionArgs func)
    assertEqual "Count arguments of non-function AST"
        0
        (countFunctionArgs (AstInt 42))

testIsValidCondition :: Test
testIsValidCondition = TestCase $ do
    assertBool "Boolean condition is valid" (isValidCondition (AstBool True))
    assertBool "Function condition is valid" (isValidCondition (AstFunction (Function "f" [])))
    assertBool "Non-condition AST is invalid" (not $ isValidCondition (AstInt 42))

testReplaceParam :: Test
testReplaceParam = TestCase $ do
    let target = AstFunction (Function "f" [AstStr "x"])
        param = AstStr "x"
        value = AstInt 42
        expected = AstFunction (Function "f" [AstInt 42])
        result = replaceParam target param value
    case result of
        AstFunction (Function fName args)
            | fName == "f" && args == [AstInt 42] -> return ()
        _ -> assertFailure "Replace parameter in function arguments: Result does not match expected output"

testChangeValLambda :: Test
testChangeValLambda = TestCase $ do
    let lambda = AstLambda (Lambda [AstStr "x"] (AstFunction (Function "f" [AstStr "x"])))
        newValues = [AstInt 42]
        expected = Right (AstFunction (Function "f" [AstInt 42]))
        result = changeValLambda lambda newValues
    case result of
        Right (AstFunction (Function fName args))
            | fName == "f" && args == [AstInt 42] -> return ()
        _ -> assertFailure "Change parameter value in lambda: Result does not match expected output"

testEvalOpMathFunc :: Test
testEvalOpMathFunc = TestCase $ do
    let result = evalOpMathFunc [] "+" [AstInt 1, AstInt 2] checkFunction (+)
    case result of
        Right ([], AstInt 3) -> return ()
        _ -> assertFailure "Evaluate addition operation: Result does not match expected output"


testEvalASTWithDefine :: Test
testEvalASTWithDefine = TestCase $ do
    let defines = [Define "x" (AstInt 10)]
    case evalAST defines (AstFunction (Function "+" [AstInt 10, AstInt 10])) of
        Right (_, AstInt 20) -> return ()
        _ -> assertFailure "evalAST failed to evaluate addition with define"
    case evalAST defines (AstDefine (Define "y" (AstInt 20))) of
        Right (newDefines, _) ->
            if length newDefines == 2
                then return ()
                else assertFailure "Define not added correctly"
        _ -> assertFailure "evalAST failed to add new define"

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
    TestLabel "testFindDefine" testFindDefine,
    TestLabel "testSexprToAST" testSexprToAST,
    TestLabel "testEvalAST" testEvalAST,
    TestLabel "testEvalASTWithDefine" testEvalASTWithDefine,
    TestLabel "testCountFunctionArgs" testCountFunctionArgs,
    TestLabel "testIsValidCondition" testIsValidCondition,
    TestLabel "testReplaceParam" testReplaceParam,
    TestLabel "testChangeValLambda" testChangeValLambda,
    TestLabel "testEvalOpMathFunc" testEvalOpMathFunc
  ]

main :: IO ()
main = do
    counts <- runTestTT tests
    print counts
    if errors counts + failures counts > 0
        then error "Tests failed!"
        else return ()

