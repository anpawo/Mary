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