{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizer
-}

{-# LANGUAGE LambdaCase #-}

module Opti.Optimizer 
  ( optimizeAST
  , reachableFunctions
  , collectCallsAst
  , collectCallsExpr
  , eliminateUnusedFuncs
  , optimizeExpr
  , fixOptSubExpr
  , optimizeSubExpr
  , collectCallsSubExpr
  ) where

import Ast.Ast
import Parser.Token (Literal(..))
import qualified Data.Set as Set
import Data.Function ()

optimizeAST :: [Ast] -> [Ast]
optimizeAST = eliminateUnusedFuncs . map (eliminateUnusedVars . optimizeExprInAst . optimizeAst)

optimizeAst :: Ast -> Ast
optimizeAst f@Function { fnBody = body } = f { fnBody = map optimizeExpr body }
optimizeAst other                     = other

optimizeExpr :: Expression -> Expression
optimizeExpr = \case
  SubExpression sub    -> SubExpression (fixOptSubExpr sub)
  IfThenElse cond t e  -> IfThenElse (fixOptSubExpr cond) (map optimizeExpr t) (map optimizeExpr e)
  While cond body      -> While (fixOptSubExpr cond) (map optimizeExpr body)
  Variable meta val    -> Variable meta (fixOptSubExpr val)
  other                -> other

fixOptSubExpr :: SubExpression -> SubExpression
fixOptSubExpr = until (\sub -> optimizeSubExpr sub == sub) optimizeSubExpr

optimizeSubExpr :: SubExpression -> SubExpression
optimizeSubExpr fc@(FunctionCall { fnCallName = op, fnCallArgs = args }) =
  case (op, newArgs) of
    ("+", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x + y))
    ("-", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x - y))
    ("*", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x * y))
    ("/", [Lit (IntLit x), Lit (IntLit y)]) -> if y == 0 then fc { fnCallArgs = newArgs } else Lit (IntLit (x `div` y))
    (_, as) -> fc { fnCallArgs = as }
  where
    newArgs = map fixOptSubExpr args
optimizeSubExpr other = other

optimizeExprInAst :: Ast -> Ast
optimizeExprInAst f@Function { fnBody = body } = f { fnBody = map optimizeExpr body }
optimizeExprInAst other                     = other

collectCallsExpr :: Expression -> [String]
collectCallsExpr (SubExpression sub)   = collectCallsSubExpr sub
collectCallsExpr (IfThenElse cond t e) = collectCallsSubExpr cond ++ concatMap collectCallsExpr t ++ concatMap collectCallsExpr e
collectCallsExpr (While cond body)     = collectCallsSubExpr cond ++ concatMap collectCallsExpr body
collectCallsExpr (Variable _ val)      = collectCallsSubExpr val
collectCallsExpr _                     = []

collectCallsSubExpr :: SubExpression -> [String]
collectCallsSubExpr (FunctionCall { fnCallName = name, fnCallArgs = args }) =
  name : concatMap collectCallsSubExpr args
collectCallsSubExpr (Lit _)            = []
collectCallsSubExpr (VariableCall name)= [name]

collectCallsAst :: Ast -> [String]
collectCallsAst (Function { fnBody = body }) = concatMap collectCallsExpr body
collectCallsAst _                            = []

reachableFunctions :: [Ast] -> Set.Set String
reachableFunctions asts =
  until (\acc -> Set.null (new acc))
        (\acc -> Set.union acc (new acc))
        (Set.singleton "main")
  where
    new acc = Set.fromList [ name
                           | ast  <- asts
                           , name <- collectCallsAst ast
                           , not (Set.member name acc)
                           ]

eliminateUnusedFuncs :: [Ast] -> [Ast]
eliminateUnusedFuncs asts =
  filter (\ast -> case ast of
                    Function { fnName = name } -> Set.member name (reachableFunctions asts)
                    _ -> True
         ) asts

eliminateUnusedVars :: Ast -> Ast
eliminateUnusedVars f@(Function { fnBody = body }) = 
  f { fnBody = filter keepVar body }
  where
    usedVars = concatMap collectCallsExpr body
    keepVar = \case
      Variable (_, name) _ -> name `elem` usedVars
      _                    -> True
eliminateUnusedVars ast = ast
