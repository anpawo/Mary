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