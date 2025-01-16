{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

{-# LANGUAGE LambdaCase #-}

module Opti.Optimizer (optimizeAST) where

import Ast.Ast
import Parser.Token (Literal(..))
import qualified Data.Set as Set

optimizeAST :: [Ast] -> [Ast]
optimizeAST asts =
  let optimized = map optimizeAst asts
      optimized' = map optimizeExprInAst optimized
      pruned = eliminateUnused optimized'
  in pruned

optimizeAst :: Ast -> Ast
optimizeAst fun@(Function { fnBody = body }) =
  fun { fnBody = map optimizeExpr body }
optimizeAst other = other

optimizeExpr :: Expression -> Expression
optimizeExpr = \case
  SubExpression sub -> SubExpression (fixOptSubExpr sub)
  IfThenElse cond thenEx elseEx ->
    IfThenElse (fixOptSubExpr cond) (map optimizeExpr thenEx) (map optimizeExpr elseEx)
  While cond body ->
    While (fixOptSubExpr cond) (map optimizeExpr body)
  Variable meta val -> Variable meta (fixOptSubExpr val)
  other -> other

fixOptSubExpr :: SubExpression -> SubExpression
fixOptSubExpr sub =
  let sub' = optimizeSubExpr sub
  in if sub' == sub then sub' else fixOptSubExpr sub'

optimizeSubExpr :: SubExpression -> SubExpression
optimizeSubExpr fc@(FunctionCall { fnCallName = op, fnCallArgs = args }) =
  let newArgs = map fixOptSubExpr args
      newNode = fc { fnCallArgs = newArgs }
  in case (op, newArgs) of
       ("+", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x + y))
       ("-", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x - y))
       ("*", [Lit (IntLit x), Lit (IntLit y)]) -> Lit (IntLit (x * y))
       ("/", [Lit (IntLit x), Lit (IntLit y)]) ->
         if y == 0 then newNode else Lit (IntLit (x `div` y))
       _ -> newNode
optimizeSubExpr other = other

optimizeExprInAst :: Ast -> Ast
optimizeExprInAst fun@(Function { fnBody = body }) =
  fun { fnBody = map optimizeExpr body }
optimizeExprInAst other = other

collectCallsExpr :: Expression -> [String]
collectCallsExpr (SubExpression sub) = collectCallsSubExpr sub
collectCallsExpr (IfThenElse cond thenEx elseEx) =
  collectCallsSubExpr cond ++ concatMap collectCallsExpr thenEx ++ concatMap collectCallsExpr elseEx
collectCallsExpr (While cond body) =
  collectCallsSubExpr cond ++ concatMap collectCallsExpr body
collectCallsExpr (Variable _ val) = collectCallsSubExpr val
collectCallsExpr _ = []

collectCallsSubExpr :: SubExpression -> [String]
collectCallsSubExpr (FunctionCall { fnCallName = name, fnCallArgs = args }) =
  name : concatMap collectCallsSubExpr args
collectCallsSubExpr (Lit _) = []
collectCallsSubExpr (VariableCall name) = [name]
  
collectCallsAst :: Ast -> [String]
collectCallsAst (Function { fnBody = body }) = concatMap collectCallsExpr body
collectCallsAst _ = []

reachableFunctions :: [Ast] -> Set.Set String
reachableFunctions asts =
  let _ = [ fnName f | f@(Function {}) <- asts ]
      go :: Set.Set String -> Set.Set String
      go acc =
        let new = Set.fromList [ name
                               | ast <- asts
                               , let cs = collectCallsAst ast
                               , name <- cs
                               , not (Set.member name acc)
                               ]
        in if Set.null new then acc else go (Set.union acc new)
  in go (Set.singleton "main")

eliminateUnused :: [Ast] -> [Ast]
eliminateUnused asts =
  let reachable = reachableFunctions asts
  in filter (\ast -> case ast of
                       Function { fnName = name } -> Set.member name reachable
                       _ -> True) asts
