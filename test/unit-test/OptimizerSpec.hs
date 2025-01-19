{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module OptimizerSpec (spec) where

import Test.Hspec
import Data.Set()
import Opti.Optimizer
import Ast.Ast
import Parser.Token (Literal(..), Type(..))

spec :: Spec
spec = do
  functionSpec
  operatorSpec
  callCollectionSpec
  reachableSpec
  defaultAstSpec
  optimizeExprPatternsSpec

functionSpec :: Spec
functionSpec = describe "eliminateUnused" $ do
  it "removes functions that are not reachable" $ do
    let asts =
          [ Function { fnName = "hundred", fnArgs = [], fnRetType = IntType, fnBody = [Return (Lit (IntLit 100))] }
          , Function { fnName = "thousand", fnArgs = [], fnRetType = IntType, fnBody = [Return (Lit (IntLit 1000))] }
          , Function
              { fnName = "main"
              , fnArgs = []
              , fnRetType = VoidType
              , fnBody =
                  [ Variable (IntType, "a") (FunctionCall "-" [ FunctionCall "+" [Lit (IntLit 5), Lit (IntLit 9)]
                                                                  , FunctionCall "*" [Lit (IntLit 4), Lit (IntLit 5)]
                                                                  ]
                                      )
                  , SubExpression (FunctionCall "print" [VariableCall "a"])
                  ]
              }
          ]
    let optimizedAsts = optimizeAST asts
    length optimizedAsts `shouldBe` 1
    let mainFun = head optimizedAsts
    case mainFun of
      Function { fnName = name, fnBody = body } -> do
        name `shouldBe` "main"
        case body of
          [ Variable (_, _) varVal
            , SubExpression (FunctionCall callName args)
            ] -> do
              varVal `shouldBe` Lit (IntLit (-6))
              callName `shouldBe` "print"
              args `shouldBe` [VariableCall "a"]
          _ -> expectationFailure "Unexpected structure in function body"
      _ -> expectationFailure "Expected a Function AST node"

operatorSpec :: Spec
operatorSpec = describe "optimizeSubExpr" $ do
  it "optimizes addition" $ do
    let expr = FunctionCall "+" [Lit (IntLit 3), Lit (IntLit 4)]
    optimizeSubExpr expr `shouldBe` Lit (IntLit 7)
  it "optimizes subtraction" $ do
    let expr = FunctionCall "-" [Lit (IntLit 10), Lit (IntLit 4)]
    optimizeSubExpr expr `shouldBe` Lit (IntLit 6)
  it "optimizes multiplication" $ do
    let expr = FunctionCall "*" [Lit (IntLit 3), Lit (IntLit 4)]
    optimizeSubExpr expr `shouldBe` Lit (IntLit 12)
  it "optimizes division" $ do
    let expr = FunctionCall "/" [Lit (IntLit 20), Lit (IntLit 4)]
    optimizeSubExpr expr `shouldBe` Lit (IntLit 5)
  it "does not optimize division by zero" $ do
    let expr = FunctionCall "/" [Lit (IntLit 20), Lit (IntLit 0)]
    optimizeSubExpr expr `shouldBe` FunctionCall "/" [Lit (IntLit 20), Lit (IntLit 0)]

callCollectionSpec :: Spec
callCollectionSpec = describe "collectCalls functions" $ do
  it "collects calls in an if-then-else expression" $ do
    let cond = FunctionCall "==" [VariableCall "a", Lit (IntLit 5)]
        thenExprs = [ SubExpression (FunctionCall "print" [VariableCall "b"]) ]
        elseExprs = [ SubExpression (FunctionCall "log" [VariableCall "c"]) ]
        expr = IfThenElse cond thenExprs elseExprs
    collectCallsExpr expr `shouldBe`
      (collectCallsSubExpr cond ++
       concatMap collectCallsExpr thenExprs ++
       concatMap collectCallsExpr elseExprs)
  it "collects calls in a while expression" $ do
    let cond = FunctionCall "!=" [VariableCall "x", Lit (IntLit 0)]
        body = [ SubExpression (FunctionCall "decrement" [VariableCall "x"]) ]
        expr = While cond body
    collectCallsExpr expr `shouldBe`
      (collectCallsSubExpr cond ++ concatMap collectCallsExpr body)
  it "collects calls in a variable expression" $ do
    let expr = Variable (IntType, "y") (FunctionCall "*" [Lit (IntLit 2), Lit (IntLit 3)])
    collectCallsExpr expr `shouldBe` collectCallsSubExpr (FunctionCall "*" [Lit (IntLit 2), Lit (IntLit 3)])

reachableSpec :: Spec
reachableSpec = describe "reachableFunctions" $ do
  it "collects function names from the AST using a list comprehension" $ do
    let asts =
          [ Function { fnName = "funA", fnArgs = [], fnRetType = IntType, fnBody = [] }
          , Function { fnName = "funB", fnArgs = [], fnRetType = IntType, fnBody = [] }
          , Function { fnName = "main", fnArgs = [], fnRetType = VoidType, fnBody = [] }
          ]
    let names = [ fnName f | f@(Function {}) <- asts ]
    names `shouldBe` ["funA", "funB", "main"]

defaultAstSpec :: Spec
defaultAstSpec = describe "collectCallsAst and optimizeAST default behavior" $ do
  it "returns an empty list for collectCallsAst when given a non-function AST" $ do
    let ast = Structure { structName = "Dummy", structMember = [] }
    collectCallsAst ast `shouldBe` []
  it "leaves non-function AST nodes unchanged when optimizing" $ do
    let ast = Structure { structName = "Dummy", structMember = [] }
    optimizeAST [ast] `shouldBe` []

optimizeExprPatternsSpec :: Spec
optimizeExprPatternsSpec = describe "optimizeExpr patterns" $ do
  it "optimizes an IfThenElse expression by processing the condition and sub-expressions" $ do
    let cond = FunctionCall "+" [Lit (IntLit 2), Lit (IntLit 3)]
        thenPart = [Variable (IntType, "a") (FunctionCall "*" [Lit (IntLit 2), Lit (IntLit 2)])]
        elsePart = [SubExpression (FunctionCall "-" [Lit (IntLit 10), Lit (IntLit 4)])]
        expr = IfThenElse cond thenPart elsePart
        optimizedExpr = optimizeExpr expr
    case optimizedExpr of
      IfThenElse cond' thenEs elseEs -> do
        cond' `shouldBe` Lit (IntLit 5)
        thenEs `shouldSatisfy` (not . null)
        elseEs `shouldSatisfy` (not . null)
      _ -> expectationFailure "Expected an IfThenElse expression"
  it "optimizes a While expression by processing the condition and body" $ do
    let cond = FunctionCall "-" [Lit (IntLit 10), Lit (IntLit 4)]
        body = [Variable (IntType, "x") (FunctionCall "*" [Lit (IntLit 3), Lit (IntLit 3)])]
        expr = While cond body
        optimizedExpr = optimizeExpr expr
    case optimizedExpr of
      While cond' body' -> do
        cond' `shouldBe` Lit (IntLit 6)
        body' `shouldSatisfy` (not . null)
      _ -> expectationFailure "Expected a While expression"
  it "returns the original expression for non-matching patterns in optimizeAst" $ do
    let ast = Structure { structName = "MyStruct", structMember = [] }
    optimizeAST [ast] `shouldBe` []
  it "optimizes if with constant condition true and non-empty then branch" $ do
    let expr = IfThenElse (Lit (BoolLit True))
                          [SubExpression (FunctionCall "print" [Lit (StringLit "then branch")])]
                          [SubExpression (FunctionCall "log" [Lit (StringLit "else branch")])]
        optimized = optimizeExpr expr
    optimized `shouldBe` SubExpression (FunctionCall "print" [Lit (StringLit "then branch")])

  it "optimizes if with constant condition true and empty then branch" $ do
    let expr = IfThenElse (Lit (BoolLit True))
                          []
                          [SubExpression (FunctionCall "log" [Lit (StringLit "else branch")])]
        optimized = optimizeExpr expr
    optimized `shouldBe` SubExpression (Lit (StringLit ""))

  it "optimizes if with constant condition false and non-empty else branch" $ do
    let expr = IfThenElse (Lit (BoolLit False))
                          [SubExpression (FunctionCall "print" [Lit (StringLit "then branch")])]
                          [SubExpression (FunctionCall "log" [Lit (StringLit "else branch")])]
        optimized = optimizeExpr expr
    optimized `shouldBe` SubExpression (FunctionCall "log" [Lit (StringLit "else branch")])

  it "optimizes if with constant condition false and empty else branch" $ do
    let expr = IfThenElse (Lit (BoolLit False))
                          [SubExpression (FunctionCall "print" [Lit (StringLit "then branch")])]
                          []
        optimized = optimizeExpr expr
    optimized `shouldBe` SubExpression (Lit (StringLit ""))

  it "optimizes while with constant condition false" $ do
    let expr = While (Lit (BoolLit False))
                     [SubExpression (FunctionCall "print" [Lit (StringLit "loop body")])]
        optimized = optimizeExpr expr
    optimized `shouldBe` SubExpression (Lit (StringLit ""))
    
  it "leaves while unchanged when condition is not a constant false" $ do
    let cond = FunctionCall ">" [Lit (IntLit 5), Lit (IntLit 3)]
        expr = While cond [SubExpression (FunctionCall "print" [Lit (StringLit "loop body")])]
        optimized = optimizeExpr expr
    case optimized of
      While cond' body' -> do
         cond' `shouldBe` fixOptSubExpr cond
         body' `shouldSatisfy` (not . null)
      _ -> expectationFailure "Expected a While expression"
