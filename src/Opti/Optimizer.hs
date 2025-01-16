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
import System.IO.Unsafe (unsafePerformIO)

optimizeAST :: [Ast] -> [Ast]
optimizeAST asts =
  unsafePerformIO $ do
    putStrLn (show asts)
    putStrLn ""
    let optimized = map optimizeAst asts
    putStrLn (show optimized)
    putStrLn ""
    return optimized

optimizeAst :: Ast -> Ast
optimizeAst fun@(Function { fnBody = body }) = fun { fnBody = map optimizeExpr body }
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