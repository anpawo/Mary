{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Sexpression
-}

module AST.Sexpression
  ( sexprToAST,
    sexprToASTCondition,
    sexprToASTDefine,
    sexprToASTLambda,
    sexprToASTLambdaWithVal,
    sexprToASTList,
  )
where

import AST.Data
import AST.Tools (changeValLambda, extractParam, isValidCondition)
import SExprParser (SExpr (..))

sexprToASTList :: [SExpr] -> Either String [AST]
sexprToASTList [] = Right []
sexprToASTList (x : xs) =
  case sexprToAST x of
    Right ast ->
      case sexprToASTList xs of
        Right astList -> Right (ast : astList)
        Left err -> Left err
    Left err -> Left ("Error in list element: " ++ err)

sexprToASTCondition :: SExpr -> SExpr -> SExpr -> Either String AST
sexprToASTCondition _condition _true _false =
  case (sexprToAST _condition, sexprToAST _true, sexprToAST _false) of
    (Right condition, Right _true, Right _false) ->
      if isValidCondition condition
        then Right (AstCondition (Condition condition _true _false))
        else Left "Error in if condition: Condition must be a boolean or a function"
    (Left err, _, _) -> Left ("Error in if condition: " ++ err)
    (_, Left err, _) -> Left ("Error in if true branch: " ++ err)
    (_, _, Left err) -> Left ("Error in if false branch: " ++ err)

sexprToASTDefine :: SExpr -> Either String AST
sexprToASTDefine (SExprList [SExprAtomString "define", SExprAtomString _name, _value]) =
  case sexprToAST _value of
    Right astValue -> Right (AstDefine (Define _name astValue))
    Left err -> Left ("Error in define value: " ++ err)
sexprToASTDefine (SExprList [SExprAtomString "define", SExprList _named_func, SExprList _func]) =
  case sexprToAST (SExprList _named_func) of
    Right (AstFunction (Function _real_name _real_name_args)) -> case sexprToAST (SExprList _func) of
      Right (AstFunction function) -> Right (AstDefine (Define _real_name (AstLambda (Lambda _real_name_args (AstFunction function)))))
      Right (AstCondition condition) -> Right (AstDefine (Define _real_name (AstLambda (Lambda _real_name_args (AstCondition condition)))))
      Right _ -> Left "Lambda body must be a function or condition"
      Left _ -> Left "Lambda must be a function"
    Left err -> Left ("Error in define value: " ++ err)

sexprToASTLambdaWithVal :: SExpr -> Either String AST
sexprToASTLambdaWithVal (SExprList (SExprList lambda : values)) = case sexprToAST (SExprList lambda) of
  Right (AstLambda (Lambda paramList body)) ->
    if length values == length paramList
      then case sexprToASTList values of
        Right parsedValues -> changeValLambda (AstLambda (Lambda paramList body)) parsedValues
        Left err -> Left ("Error in parsing values: " ++ err)
      else Left "Error: Number of arguments provided does not match the lambda argument count"
  Left err -> Left err

sexprToASTLambda :: SExpr -> Either String AST
sexprToASTLambda (SExprList [SExprAtomString "lambda", SExprList params, SExprList body]) =
  case mapM extractParam params of
    Just paramList -> case sexprToAST (SExprList body) of
      Right (AstFunction res) -> Right (AstLambda (Lambda paramList (AstFunction res)))
      Right (AstCondition res) -> Right (AstLambda (Lambda paramList (AstCondition res)))
      Right _ -> Left "Lambda body must be a function"
      Left err -> Left ("Error in lambda body: " ++ err)
    Nothing -> Left "Error parsing lambda parameters"

sexprToAST :: SExpr -> Either String AST
sexprToAST (SExprAtomInt num) = Right (AstInt num)
sexprToAST (SExprAtomString "#t") = Right (AstBool True)
sexprToAST (SExprAtomString "#f") = Right (AstBool False)
sexprToAST (SExprAtomString str) = Right (AstStr str)
sexprToAST (SExprList (SExprAtomString "define" : rest)) = sexprToASTDefine (SExprList (SExprAtomString "define" : rest))
sexprToAST (SExprList [SExprAtomString "if", _condition, _true, _false]) = sexprToASTCondition _condition _true _false
sexprToAST (SExprList [SExprAtomString "lambda", SExprList params, SExprList body]) = sexprToASTLambda (SExprList [SExprAtomString "lambda", SExprList params, SExprList body])
sexprToAST (SExprList (SExprAtomString _name : args)) = case sexprToASTList args of
  Right parsedArgs -> Right (AstFunction (Function _name parsedArgs))
  Left err -> Left ("Error in function parse args" ++ err)
sexprToAST (SExprList (SExprList lambda : values)) = sexprToASTLambdaWithVal (SExprList (SExprList lambda : values))
sexprToAST _ = Left "Unrecognized SExpr"