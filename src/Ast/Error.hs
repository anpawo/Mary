{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}

module Ast.Error (errCondNotBool, errAssignType, errNameTaken, errImpossibleCase, prettyPrintError, errExpectedType, errTopLevelDef, errStartBody, errTodo, errEndBody, errVoidRet, errRetType, errEndSubexpr, errInvalidExprToken, errEmptyParen, errEmptyExpr, errOpNotDefined, errMissingOperand, errTooManyExpr, errVariableNotBound, errFunctionNotBound, errInvalidNumberOfArgument, errOperatorNotBound, errInvalidVarType, errInvalidFnType, errInvalidLitType, errInvalidOpType, errOpArgs, errSemiColon, errStructureNotBound, errInvalidStructure, errInvalidArray, errConstraintNotBound, bgBlack, errStructureFieldNotBound, errMissingRetT, errExpectedField) where

import Text.Printf (printf)
import Text.Megaparsec.Error (ParseErrorBundle(..), ParseError(..), ErrorFancy(..))
import Parser.Token

import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.Internal (elemAt)
import Data.List (intercalate)

errCondNotBool :: String -> String
errCondNotBool name = printf "Condition in '%s' must evaluate to a boolean type" $ purple name

errNameTaken :: String -> String
errNameTaken name = printf "name '%s' already taken." $ purple name

errExpectedType :: Bool -> String
errExpectedType canBeVoid
    | canBeVoid = printf "expected a %s (including void)." $ purple "type"
    | otherwise = printf "expected a %s (excluding void)." $ purple "type"

errStartBody :: String
errStartBody = printf "expected a '%s' representing the start of the body of the function." $ purple "{"

errEndBody :: String
errEndBody = printf "expected a '%s' representing the end of the body of the function." $ purple "}"

errTodo :: String -> String
errTodo = printf "todo: %s." . purple

errVoidRet :: String
errVoidRet = printf "%s function should not %s." (purple "void") (purple "return")

errEndSubexpr :: String
errEndSubexpr = printf "expected end of expression '%s'." $ purple ";"

errEmptyParen :: String
errEmptyParen = printf ":2expected an expression inside the %s." $ purple "parenthesis"

errEmptyExpr :: String
errEmptyExpr = "expected an expression."

errExpectedField :: String
errExpectedField = "expected a structure field."

errMissingRetT :: String
errMissingRetT = printf "expected return type: '%s'." (purple "-> <type>")

errSemiColon :: String
errSemiColon = printf "expected a '%s' at the end of the expression." (purple ";")

errOpNotDefined :: String -> String
errOpNotDefined = printf "the operator '%s' is not defined." . purple

errMissingOperand :: String -> String -> String
errMissingOperand side name = printf "missing the %s operand for the operator '%s'." (purple side) (purple name)

errInvalidExprToken :: MyToken -> String
errInvalidExprToken = printf "invalid expression '%s'." . purple . show

errTooManyExpr :: Int -> String
errTooManyExpr = printf ":%stoo many expressions, expected one." . show

errOpArgs :: Int -> Int -> String -> String
errOpArgs lenErr nargs name = printf ":%soperators must take 2 arguments. '%s' has %s argument%s." (show lenErr) (purple name) (purple . show $ nargs) (if nargs <= 1 then "" else "s")

errRetType :: String -> String -> String
errRetType expected got = printf "invalid return type, expected '%s' got '%s'." (purple expected) (purple got)

errAssignType :: String -> String -> String -> String
errAssignType name expected got = printf "invalid type for the variable '%s', expected '%s' got '%s'." (purple name) (purple expected) (purple got)

errTopLevelDef :: String
errTopLevelDef = printf "top level declaration must be either %s, %s or %s." (purple "function") (purple "operator") (purple "struct")

errImpossibleCase :: String -> String
errImpossibleCase = printf "Impossible case. (from %s)." . purple

errVariableNotBound :: String -> String
errVariableNotBound = printf "variable '%s' is not bound." . purple

errFunctionNotBound :: String -> String
errFunctionNotBound = printf "function '%s' doesn't exist." . purple

errStructureNotBound :: String -> String
errStructureNotBound = printf "structure '%s' doesn't exist." . purple

errStructureFieldNotBound :: String -> String -> String
errStructureFieldNotBound name field = printf "structure '%s' doesn't have the field '%s'." (purple name) (purple field)

errConstraintNotBound :: String -> String
errConstraintNotBound = printf "constraint '%s' doesn't exist." . purple

errInvalidStructure :: String -> [(String, Type)] -> String
errInvalidStructure st diff = printf "invalid structure '%s', expected:\n{\n    %s\n}" (purple st) $ intercalate ",\n    " $ map (\(n, t) -> printf "%s = %s" (purple n) (purple $ show t)) diff

errInvalidArray :: String -> String
errInvalidArray = printf "invalid array of type '%s'." . purple

errOperatorNotBound :: String -> String
errOperatorNotBound = printf "operator '%s' is not bound." . purple

errInvalidLitType :: String -> String -> String
errInvalidLitType expected got = printf "invalid literal type, expected %s but got %s." (purple expected) (purple got)

errInvalidVarType :: String -> String -> String -> String
errInvalidVarType name expected got = printf "invalid variable type for '%s', expected '%s' but got '%s'." (purple name) (purple expected) (purple got)

errInvalidFnType :: String -> String -> String -> String
errInvalidFnType name expected got = printf "invalid function type for '%s', expected '%s' but got '%s'." (purple name) (purple expected) (purple got)

errInvalidOpType :: String -> String -> String -> String
errInvalidOpType name expected got = printf "invalid operator type for '%s', expected '%s' but got '%s'." (purple name) (purple expected) (purple got)

errInvalidNumberOfArgument :: String -> Int -> Int -> String
errInvalidNumberOfArgument name expected found = printf "invalid number of arguments for the function '%s', expected %s but found %s." (purple name) (purple . show $ expected) (purple . show $ found)

prettyPrintError :: [MyToken] -> ParseErrorBundle [MyToken] Void -> String
prettyPrintError tokens (ParseErrorBundle {bundleErrors = errors, bundlePosState = _}) =
    case errors of
        (FancyError pos fancySet :| _) ->
            case 0 `elemAt` fancySet of
                (ErrorFail (';': err)) -> err
                (ErrorFail (':': input)) -> do
                    (n, err) <- (reads input :: [(Int, String)])
                    " |\n | " ++ tokCons ++ red (tokErr2 n) ++ tokLeft2 n ++ "\n |" ++ red (pointer2 n) ++ "\n" ++ red "error" ++ ": " ++ err
                (ErrorFail err) ->
                    " |\n | " ++ tokCons ++ red tokErr ++ tokLeft ++ "\n |" ++ red pointer ++ "\n" ++ red "error" ++ ": " ++ err
                x -> "This error should be transformed into a custom one:\n" ++ show x
            where
                tokCons = unwords $ show <$> suffix (take (pos - 1) tokens)
                tokErr
                    | pos > 0 = " " ++ show (tokens !! (pos - 1)) ++ " "
                    | otherwise = ""
                tokLeft = let tc = take 10 (drop pos tokens) in unwords $ show <$> tc
                pointer = replicate (length tokCons + 2) ' ' ++ replicate (length tokErr - 2) '^'

                tokErr2 n = (\s -> " " ++ s ++ " ") <$> unwords $ show <$> take n (drop (pos - 1) tokens)
                tokLeft2 n = let tc = take 10 (drop (pos + n - 1) tokens) in unwords $ show <$> tc
                pointer2 n = replicate (length tokCons + 2) ' ' ++ replicate (length (tokErr2 n) - 2) '^'

                suffix :: [a] -> [a]
                suffix l
                    | len < 10 = l
                    | otherwise = drop (len - 10) l
                    where len = length l
        (TrivialError offset unexpected expected :| _) ->
            printf "This error should be transformed into a custom one:\n\nerror token: %s\nunexpected: %s\nexpected: %s\n" (show $ tokens !! offset) (show unexpected) (show expected)

red :: String -> String
red s = "\ESC[91m" ++ s ++ reset

purple :: String -> String
purple s = "\ESC[95m" ++ s ++ reset

reset :: String
reset = "\ESC[0m"

bgBlack :: String -> String
bgBlack s = "\ESC[48;2;10;10;10m" ++ s ++ reset
