{- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Optimizer
-}

module Opti.Optimizer (optimizeAST) where

import Ast.Ast
import Parser.Token (Literal(..))

printDebug :: Bool
printDebug = True -- if true prints ast before and after optimization

optimizeAST :: [Ast] -> IO [Ast]
optimizeAST asts = do
    if printDebug then print asts else return ()
    let astsOpt = map optimizeAst asts
    if printDebug then print astsOpt else return ()
    return astsOpt

optimizeAst :: Ast -> Ast
optimizeAst fun@(Function { fnBody = body }) = fun { fnBody = map optimizeExpr body }
optimizeAst other = other

optimizeExpr :: Expression -> Expression
optimizeExpr (SubExpression sub) = SubExpression (optimizeSubExpr sub)
optimizeExpr (IfThenElse cond thenEx elseEx) =
  IfThenElse (optimizeSubExpr cond) (map optimizeExpr thenEx) (map optimizeExpr elseEx)
optimizeExpr (While cond body) =
  While (optimizeSubExpr cond) (map optimizeExpr body)
optimizeExpr (Variable meta val) = Variable meta (optimizeSubExpr val)
optimizeExpr other = other

optimizeSubExpr :: SubExpression -> SubExpression
optimizeSubExpr fc@(FunctionCall { fnCallName = "+", fnCallArgs = args }) =
  case args of
    [Lit (IntLit x), Lit (IntLit y)] -> Lit (IntLit (x + y))
    _ -> fc { fnCallArgs = map optimizeSubExpr args }
optimizeSubExpr fc@(FunctionCall { fnCallName = "-", fnCallArgs = args }) =
  case args of
    [Lit (IntLit x), Lit (IntLit y)] -> Lit (IntLit (x - y))
    _ -> fc { fnCallArgs = map optimizeSubExpr args }
optimizeSubExpr fc@(FunctionCall { fnCallName = "*", fnCallArgs = args }) =
  case args of
    [Lit (IntLit x), Lit (IntLit y)] -> Lit (IntLit (x * y))
    _ -> fc { fnCallArgs = map optimizeSubExpr args }
optimizeSubExpr fc@(FunctionCall { fnCallName = "/", fnCallArgs = args }) =
  case args of
    [Lit (IntLit x), Lit (IntLit y)] ->
      if y == 0
         then fc
         else Lit (IntLit (x `div` y))
    _ -> fc { fnCallArgs = map optimizeSubExpr args }
optimizeSubExpr fc@(FunctionCall { fnCallArgs = args }) =
  fc { fnCallArgs = map optimizeSubExpr args }
optimizeSubExpr other = other