{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Check
-}

module AST.Check
  ( checkFunction,
    checkDivid,
    checkBool,
    findDefine,
  ) where

import AST.Data
import AST.Eval

findDefine :: [Define] -> String -> Either String AST
findDefine list_define nameToFind =
    case filter (\def -> name def == nameToFind) list_define of
        (Define _ defineValue : _) -> Right defineValue
        [] -> Left ("Define with name '" ++ nameToFind ++ "' not found")

checkFunction :: [Define] -> String -> [AST] -> Either String [AST]
checkFunction _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkFunction list_define _name [x , y] = case evalAST list_define x of
    Right (_, AstInt val_x) -> case evalAST list_define y of
        Right (_, AstInt val_y) -> Right [AstInt val_x, AstInt val_y]
        Left _ -> Left ("Bad number or type of arguments for " ++ _name ++ " params: " ++ show x ++ " " ++ show y)
    Left _ -> Left ("Bad number or type of arguments for " ++ _name ++ " params: " ++ show x ++ " " ++ show y)
checkFunction _ _name _ = Left ("Bad number or type of arguments for " ++ _name)

checkDivid :: [Define] -> String -> [AST] -> Either String [AST]
checkDivid _ _name [AstInt x, AstInt y] = Right [AstInt x, AstInt y]
checkDivid list_define _name [x , y] = case evalAST list_define x of
    Right (_, AstInt val_x) -> case evalAST list_define y of
        Right (_, AstInt val_y) -> Right [AstInt val_x, AstInt val_y]
        Right (_, AstInt 0) -> Left "Division by 0 is forbidden"
        Left _ -> Left ("Bad number or type of arguments for " ++ _name)
    Left _ -> Left ("Bad number or type of arguments for " ++ _name)
checkDivid _ _name _ = Left ("Bad number or type of arguments for " ++ _name)

checkBool :: [Define] -> [AST] -> Either String [AST]
checkBool list_define [x, y] = case evalAST list_define x of
    Right (_, val_x) -> case evalAST list_define y of
        Right (_, val_y) -> Right [val_x, val_y]
        Left err -> Left err
    Left err -> Left err
checkBool _ _ = Left "need only 2 arguments"
