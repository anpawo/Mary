{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}

module Ast.Error (errNameTaken, errImpossibleCase, prettyPrintError, errExpectedType, errTopLevelDef, errExpectedStartBody, errTodo, errExpectedEndBody, errVoidRet, errRetType, errEndSubexpr, errInvalidExprToken, errEmptyGroup, errEmptyExpr, errOpNotDefined, errMissingOperand) where

import Text.Megaparsec.Error (ParseErrorBundle(..), ParseError(..), ErrorFancy(..))
import Parser.Token
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.Internal (elemAt)

errNameTaken :: String -> String
errNameTaken name = "name '" ++ purple name ++ "' already taken."

errExpectedType :: Bool -> String
errExpectedType voidOk
    | voidOk = "expected a " ++ purple "type" ++ " (including void)."
    | otherwise = "expected a " ++ purple "type" ++ " (excluding void)."

errExpectedStartBody :: String
errExpectedStartBody = "expected a '" ++ purple "{" ++ "' representing the start of the " ++ purple "body" ++ " of the function."

errExpectedEndBody :: String
errExpectedEndBody = "expected a '" ++ purple "}" ++ "' representing the end of the " ++ purple "body" ++ " of the function."

errTodo :: String -> String
errTodo s = "todo: " ++ purple s ++ "."

errVoidRet :: String
errVoidRet = purple "void" ++ " function should not " ++ purple "return" ++ "."

errEndSubexpr :: String
errEndSubexpr = "expected end of expression '" ++ purple ";" ++ "'."

errEmptyGroup :: String
errEmptyGroup = "groups '" ++ purple "(...)" ++ "' cannot be empty."

errEmptyExpr :: String
errEmptyExpr = "expressions '" ++ purple "...;" ++ "' cannot be empty."

errOpNotDefined :: String -> String
errOpNotDefined op = "the operator '" ++ purple op ++ "' is not defined."

errMissingOperand :: String -> String -> String
errMissingOperand side name = "missing the " ++ purple side ++ " operand for the operator '" ++ purple name ++ "'."

errInvalidExprToken :: MyToken -> String
errInvalidExprToken t = "invalid expression '" ++ purple (show t) ++ "'."

errRetType :: String -> String -> String
errRetType expected got = "invalid " ++ purple "return type" ++ " expected " ++ purple expected ++ " got " ++ purple got ++ "."

errTopLevelDef :: String
errTopLevelDef = "top level declaration must be " ++ purple "function" ++ ", " ++ purple "operator" ++ " or " ++ purple "struct."

errImpossibleCase :: String
errImpossibleCase = "Impossible case."

prettyPrintError :: [MyToken] -> ParseErrorBundle [MyToken] Void -> String
prettyPrintError tokens (ParseErrorBundle {bundleErrors = errors, bundlePosState = _}) =
    case errors of
        (FancyError pos fancySet :| _) ->
            case 0 `elemAt` fancySet of
                (ErrorFail err) -> " |\n | " ++ tokCons ++ red tokErr ++ tokLeft ++ "\n |" ++ red pointer ++ "\n" ++ red "error" ++ ": " ++ err
                x -> "This error should be transformed into a custom one:\n" ++ show x
            where
                tokCons = unwords $ show <$> suffix (take (pos - 1) tokens)
                tokErr
                    | pos > 0 = " " ++ show (tokens !! (pos - 1)) ++ " "
                    | otherwise = ""
                tokLeft = let tc = take 3 (drop pos tokens) in unwords $ show <$> tc
                pointer = replicate (length tokCons + 2) ' ' ++ replicate (length tokErr - 2) '^'
        x -> "This error should be transformed into a custom one:\n" ++ show x

suffix :: [a] -> [a]
suffix l
    | len < 10 = l
    | otherwise = drop (len - 10) l
    where len = length l

red :: String -> String
red s = "\ESC[91m" ++ s ++ reset

purple :: String -> String
purple s = "\ESC[95m" ++ s ++ reset

reset :: String
reset = "\ESC[0m"
