{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Optimizer
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
import Parser.Token (Literal(..), Type (..))
import qualified Data.Set as Set
import Data.Function ()
import Data.List (nub, singleton)

optimizeAST :: [Ast] -> [Ast]
optimizeAST = deleteUnused . map (eliminateUnusedVars . optimizeExprInAst . optimizeAst)

optimizeAst :: Ast -> Ast
optimizeAst f@Function { fnBody = body } = f { fnBody = map optimizeExpr body }
optimizeAst o@Operator { opBody = body } = o { opBody = map optimizeExpr body }
optimizeAst other                        = other

optimizeExpr :: Expression -> Expression
optimizeExpr = \case
  IfThenElse cond thenEx elseEx ->
    case fixOptSubExpr cond of
      Lit (BoolLit True)  -> if null thenEx then SubExpression (Lit (StringLit "")) else head thenEx
      Lit (BoolLit False) -> if null elseEx then SubExpression (Lit (StringLit "")) else head elseEx
      c -> IfThenElse c (map optimizeExpr thenEx) (map optimizeExpr elseEx)
  While cond body ->
    case fixOptSubExpr cond of
      Lit (BoolLit False) -> SubExpression (Lit (StringLit ""))
      c -> While c (map optimizeExpr body)
  SubExpression sub    -> SubExpression (fixOptSubExpr sub)
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
  where newArgs = map fixOptSubExpr args
optimizeSubExpr other = other

optimizeExprInAst :: Ast -> Ast
optimizeExprInAst f@Function { fnBody = body } = f { fnBody = map optimizeExpr body }
optimizeExprInAst o@Operator { opBody = body } = o { opBody = map optimizeExpr body }
optimizeExprInAst other                     = other

collectCallsExpr :: Expression -> [String]
collectCallsExpr (SubExpression sub)   = collectCallsSubExpr sub
collectCallsExpr (IfThenElse cond t e) = collectCallsSubExpr cond ++ concatMap collectCallsExpr t ++ concatMap collectCallsExpr e
collectCallsExpr (While cond body)     = collectCallsSubExpr cond ++ concatMap collectCallsExpr body
collectCallsExpr (Variable _ val)      = collectCallsSubExpr val
collectCallsExpr (Return sub)          = collectCallsSubExpr sub
collectCallsExpr (StructField {..})    = collectCallsSubExpr fieldValue

collectCallsSubExpr :: SubExpression -> [String]
collectCallsSubExpr (FunctionCall { fnCallName = name, fnCallArgs = args }) = name : concatMap collectCallsSubExpr args
collectCallsSubExpr (VariableCall name) = [name]
collectCallsSubExpr (Lit (LambdaLit {..}))     = collectCallsSubExpr lambdaBody
collectCallsSubExpr (Lit (ClosureLit n _ _))   = [n]
collectCallsSubExpr (Lit (StructLit _ fields)) = concatMap (collectCallsSubExpr . snd) fields
collectCallsSubExpr (Lit (ArrLit _ values))    = concatMap collectCallsSubExpr values
collectCallsSubExpr (Lit _) = []

collectCallsAst :: Ast -> [String]
collectCallsAst (Function { fnBody = body }) = concatMap collectCallsExpr body
collectCallsAst (Operator { opBody = body }) = concatMap collectCallsExpr body
collectCallsAst _                            = []

reachableFunctions :: [Ast] -> Set.Set String
reachableFunctions asts =
  until (Set.null . new)
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
  filter (\case
            Function { fnName = name } -> Set.member name (reachableFunctions asts)
            Operator { opName = name } -> Set.member name (reachableFunctions asts)
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
eliminateUnusedVars o@(Operator { opBody = body }) =
  o { opBody = filter keepVar body }
  where
    usedVars = concatMap collectCallsExpr body
    keepVar = \case
      Variable (_, name) _ -> name `elem` usedVars
      _                    -> True
eliminateUnusedVars ast = ast

-- tree search

getMainFn :: [Ast] -> Maybe Ast
getMainFn [] = Nothing
getMainFn (main@(Function { fnName = "main" }):_) = Just main
getMainFn (_:xs) = getMainFn xs

getUsedAst :: Ast -> [String]
getUsedAst (Function {..})   = fnName : getUsedCustomType fnRetType ++ concatMap (getUsedCustomType . fst) fnArgs ++ concatMap getUsedAstFromExpr fnBody
getUsedAst (Operator {..})   = opName : getUsedCustomType opRetType ++ getUsedCustomType (fst opArgLeft) ++ getUsedCustomType (fst opArgRight) ++ concatMap getUsedAstFromExpr opBody
getUsedAst (Structure {..})  = structName : concatMap (getUsedCustomType . snd) structMember
getUsedAst (Constraint {..}) = constrName : concatMap getUsedCustomType constrType

getUsedCustomType :: Type -> [String]
getUsedCustomType (StructType {..}) = [stTyName]
getUsedCustomType (ConstraintType {..}) = maybe [] singleton crTyName ++ concatMap getUsedCustomType crTyTypes
getUsedCustomType (FunctionType {..}) = getUsedCustomType fnTyRet ++ concatMap getUsedCustomType fnTyArgs
getUsedCustomType (ArrType t) = getUsedCustomType t
getUsedCustomType _ = []

getUsedAstFromExpr :: Expression -> [String]
getUsedAstFromExpr (While {..}) = getUsedAstFromSubExpr whileCond ++ concatMap getUsedAstFromExpr whileExpr
getUsedAstFromExpr (IfThenElse {..}) = getUsedAstFromSubExpr ifCond ++ concatMap getUsedAstFromExpr thenExpr ++ concatMap getUsedAstFromExpr elseExpr
getUsedAstFromExpr (Return {..}) = getUsedAstFromSubExpr retValue
getUsedAstFromExpr (StructField {..}) = getUsedAstFromSubExpr fieldValue
getUsedAstFromExpr (Variable {..}) = getUsedAstFromSubExpr varValue
getUsedAstFromExpr (SubExpression x) = getUsedAstFromSubExpr x

getUsedAstFromSubExpr :: SubExpression -> [String]
getUsedAstFromSubExpr (FunctionCall {..}) = fnCallName : concatMap getUsedAstFromSubExpr fnCallArgs
getUsedAstFromSubExpr (VariableCall _) = []
getUsedAstFromSubExpr (Lit x) = getUsedAstFromLiteral x

getUsedAstFromLiteral :: Literal -> [String]
getUsedAstFromLiteral (ArrLit ty elems) = getUsedCustomType ty ++ concatMap getUsedAstFromSubExpr elems
getUsedAstFromLiteral (StructLit name fields) = name : concatMap (getUsedAstFromSubExpr . snd) fields
getUsedAstFromLiteral (ClosureLit name args retTy) = name : concatMap getUsedCustomType args ++ getUsedCustomType retTy
getUsedAstFromLiteral (LambdaLit {..}) = concatMap (getUsedCustomType . fst) lambdaArgs ++ getUsedAstFromSubExpr lambdaBody ++ getUsedCustomType lambdaRetTy
getUsedAstFromLiteral _ = []

deleteUnusedAst :: [Ast] -> [String] -> [Ast]
deleteUnusedAst [] _ = []
deleteUnusedAst (x:xs) used
  | getName x `elem` used = x : deleteUnusedAst xs used
  | otherwise             = deleteUnusedAst xs used

deleteUnused :: [Ast] -> [Ast]
deleteUnused ast = maybe [] (deleteUnusedAst ast . repeatIt ["main"] . getUsedAst) $ getMainFn ast
  where
    repeatIt :: [String] -> [String] -> [String]
    repeatIt tried new = case filter (not . (`elem` tried)) $ nub new of
      [] -> tried
      notTried -> repeatIt (tried ++ notTried) $ concatMap getUsedAst $ filter ((`elem` notTried) . getName) ast
