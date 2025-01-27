{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module defines the ErrorMessage type and related functions for handling errors in the AST.
-}

module Ast.Error (red, blue, errCondNotBool, errAssignType, errNameTaken, errImpossibleCase, prettyPrintError, errExpectedType, errTopLevelDef, errStartBody, errEndBody, errVoidRet, errRetType, errEndSubexpr, errInvalidExprToken, errEmptyParen, errEmptyExpr, errOpNotDefined, errMissingOperand, errInvalidExpr, errVariableNotBound, errFunctionNotBound, errInvalidNumberOfArgument, errOperatorNotBound, errInvalidVarType, errInvalidFnType, errInvalidLitType, errInvalidOpType, errOpArgs, errSemiColon, errStructureNotBound, errInvalidStructure, errInvalidArray, errConstraintNotBound, bgBlack, errStructureFieldNotBound, errMissingRetT, errExpectedField, colorblindMode, errMisingBody, errMisingNameFn, errMisingNameOp, errExpectedArgs, errExpectedArgName, errExpectedColon, errExpectedCommaOrParen, errEndParen, errEndExpr, errMisingPrecOp, errExpectedStartField, errExpectedFieldName, errExpectedCommaOrCurly, errMisingEqual, errMissingRet) where

import Text.Printf (printf)
import Text.Megaparsec.Error (ParseErrorBundle(..), ParseError(..), ErrorFancy(..))
import Parser.Token

import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.Internal (elemAt)
import Data.List (intercalate, isPrefixOf)
import Utils.Lib ((!?))

errCondNotBool :: Int -> String
errCondNotBool i = printf ":%sexpected a %s condition." (show i) (blue "boolean")

errNameTaken :: String -> String
errNameTaken name = printf "name '%s' already taken." $ blue name

errExpectedType :: Bool -> String
errExpectedType canBeVoid
    | canBeVoid = printf "expected a %s (including void)." $ blue "type"
    | otherwise = printf "expected a %s (excluding void)." $ blue "type"

errStartBody :: String
errStartBody = printf "expected a '%s' representing the start of the body of the function." $ blue "{"

errEndBody :: String
errEndBody = printf "expected a '%s' representing the end of the body of the function." $ blue "}"

errMissingRet :: String -> String
errMissingRet ty = printf "the last expression should return since the return type isn't '%s' but '%s'." (blue "void") (blue ty)

errVoidRet :: String
errVoidRet = printf "%s function should not %s." (blue "void") (blue "return")

errEndSubexpr :: String
errEndSubexpr = printf "expected end of expression '%s'." $ blue ";"

errEndExpr :: String
errEndExpr = printf "expected the end of the expression with '%s' or '%s'." (blue ";") (blue ")")

errEndParen :: String
errEndParen = printf "expected the end of the expression with a '%s'." $ blue ")"

errEmptyParen :: String
errEmptyParen = printf ":2expected an expression inside the %s." $ blue "parenthesis"

errEmptyExpr :: String
errEmptyExpr = "expected an expression."

errExpectedField :: String
errExpectedField = "expected a structure field."

errExpectedStartField :: String
errExpectedStartField = printf "expected a '%s' followed by the fields defition." $ blue "{"

errExpectedFieldName :: String
errExpectedFieldName = printf "expected a field name."

errMissingRetT :: String
errMissingRetT = printf "expected a '%s' followed by the return type of the function." (blue "->")

errMisingNameFn :: String
errMisingNameFn = printf "expected a name made of letters."

errMisingEqual :: String
errMisingEqual = printf "expected a '%s' followed by the types." $ blue "="

errMisingNameOp :: String
errMisingNameOp = printf "expected a name made of symbols."

errMisingPrecOp :: String
errMisingPrecOp = printf "expected the precedence as an '%s'." $ blue "integer"

errExpectedCommaOrCurly :: String
errExpectedCommaOrCurly = printf "expected a '%s' to separate the fields or a '%s' to end their definition." (blue ",") (blue "}")

errExpectedCommaOrParen :: String
errExpectedCommaOrParen = printf "expected a '%s' to separate the arguments or a '%s' to end their definition." (blue ",") (blue ")")

errExpectedColon :: String
errExpectedColon = printf "expected a '%s' to separate the name of the argument from it's type." $ blue ":"

errExpectedArgName :: String
errExpectedArgName = printf "expected the name of the argument made of letters."

errExpectedArgs :: String
errExpectedArgs = printf "expected a '%s' followed by the arguments." $ blue "("

errMisingBody :: String
errMisingBody = printf "the body of a function cannot be empty."

errSemiColon :: String
errSemiColon = printf "expected a '%s' at the end of the expression." (blue ";")

errOpNotDefined :: String -> String
errOpNotDefined = printf "the operator '%s' is not defined." . blue

errMissingOperand :: String -> String -> String
errMissingOperand side name = printf "missing the %s operand for the operator '%s'." (blue side) (blue name)

errInvalidExprToken :: MyToken -> String
errInvalidExprToken = printf "invalid expression '%s'." . blue . show

errInvalidExpr :: Int -> String
errInvalidExpr = printf ":%sinvalid expression." . show

errOpArgs :: Int -> Int -> String -> String
errOpArgs lenErr nargs name = printf ":%soperators must take 2 arguments. '%s' has %s argument%s." (show lenErr) (blue name) (blue . show $ nargs) (if nargs <= 1 then "" else "s")

errRetType :: String -> String -> String
errRetType expected got = printf "invalid return type, expected '%s' got '%s'." (blue expected) (blue got)

errAssignType :: String -> String -> String -> String
errAssignType name expected got = printf "invalid type for the variable '%s', expected '%s' got '%s'." (blue name) (blue expected) (blue got)

errTopLevelDef :: String
errTopLevelDef = printf "top level declaration must be either %s, %s, %s or %s." (blue "function") (blue "operator") (blue "struct") (blue "type")

errImpossibleCase :: String -> String
errImpossibleCase = printf "Impossible case. (from %s)." . blue

errVariableNotBound :: String -> String
errVariableNotBound = printf "variable '%s' is not bound." . blue

errFunctionNotBound :: String -> String
errFunctionNotBound = printf "function '%s' doesn't exist." . blue

errStructureNotBound :: String -> String
errStructureNotBound "empty" = printf "structure '%s' doesn't exist. (missing '%s')" (blue "empty") $ blue "import list"
errStructureNotBound "elem"  = printf "structure '%s' doesn't exist. (missing '%s')" (blue "elem") $ blue "import list"
errStructureNotBound n = printf "structure '%s' doesn't exist." $ blue n

errStructureFieldNotBound :: String -> String -> String
errStructureFieldNotBound name field = printf "structure '%s' doesn't have the field '%s'." (blue name) (blue field)

errConstraintNotBound :: String -> String
errConstraintNotBound = printf "constraint '%s' doesn't exist." . blue

errInvalidStructure :: String -> [(String, Type)] -> String
errInvalidStructure st diff = printf "invalid structure '%s', expected:\n{\n    %s\n}" (blue st) $ intercalate ",\n    " $ map (\(n, t) -> printf "%s = %s" (blue n) (blue $ show t)) diff

errInvalidArray :: String -> String
errInvalidArray = printf "invalid array of type '%s'." . blue

errOperatorNotBound :: String -> String
errOperatorNotBound = printf "operator '%s' is not bound." . blue

errInvalidLitType :: String -> String -> String
errInvalidLitType expected got = printf "invalid literal type, expected %s but got %s." (blue expected) (blue got)

errInvalidVarType :: String -> String -> String -> String
errInvalidVarType name expected got = printf "invalid variable type for '%s', expected '%s' but got '%s'." (blue name) (blue expected) (blue got)

errInvalidFnType :: String -> String -> String -> String
errInvalidFnType name expected got = printf "invalid function type for '%s', expected '%s' but got '%s'." (blue name) (blue expected) (blue got)

errInvalidOpType :: String -> String -> String -> String
errInvalidOpType name expected got = printf "invalid operator type for '%s', expected '%s' but got '%s'." (blue name) (blue expected) (blue got)

errInvalidNumberOfArgument :: String -> Int -> Int -> String
errInvalidNumberOfArgument name expected found = printf "invalid number of arguments for the function '%s', expected %s but found %s." (blue name) (blue . show $ expected) (blue . show $ found)

type Line = Int
type Col  = Int

prettyPrintError :: String -> [(Line, Col)] -> [MyToken] -> ParseErrorBundle [MyToken] Void -> String
prettyPrintError fileName tokensPos tokens (ParseErrorBundle {bundleErrors = errors, bundlePosState = _}) =
    case errors of
        (FancyError pos fancySet :| _) ->
            case 0 `elemAt` fancySet of
                (ErrorFail (';': input)) -> do
                    (n, err) <- (reads input :: [(Int, String)])
                    printf "\n%s %s" (filePosition fileName tokensPos (n - 1)) (dropWhile (/= '|') err)
                (ErrorFail (':': input)) -> do
                    (n, err) <- (reads input :: [(Int, String)])
                    printf "\n%s |\n | %s%s%s\n |%s\n%s: %s\n" (filePosition fileName tokensPos (pos - 1)) tokCons (red (tokErr2 n)) (tokLeft2 n) (red (pointer2 n)) (red "error") err
                (ErrorFail err) ->
                    printf "\n%s |\n | %s%s%s\n |%s\n%s: %s\n" (filePosition fileName tokensPos (pos - 1)) tokCons (red tokErr)       tokLeft     (red pointer)      (red "error") err
                x -> "This error should be transformed into a custom one:\n" ++ show x
            where
                tokCons = unwords $ show <$> suffix (take (pos - 1) tokens)
                tokErr = case tokens !? (pos - 1) of
                    Nothing -> " <"
                    Just t -> " " ++ show t ++ " "
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
            printf "This error should be transformed into a custom one:\nindex error: %s\nerror token: %s\nunexpected: %s\nexpected: %s\n" (show offset) (red . show $ tokens !! offset) (show unexpected) (show expected)

filePosition :: String -> [(Line, Col)] -> Int -> String
filePosition f ps idx = case ps !? idx of
    Nothing -> bold $ printf "%s:\n" f
    Just (l, c) -> bold $ printf "%s:%s:%s:\n" f (show l) (show c)

colorblindMode :: String -> String
colorblindMode [] = []
colorblindMode str@(x:xs)
    | redPrefix `isPrefixOf` str  = yellowPrefix ++ colorblindMode (drop (length yellowPrefix) str)
    | bluePrefix `isPrefixOf` str = pinkPrefix ++ colorblindMode (drop (length pinkPrefix) str)
    | otherwise                   = x : colorblindMode xs

bold :: String -> String
bold s = boldPrefix ++ s ++ reset

boldPrefix :: String
boldPrefix = "\ESC[1m"

yellowPrefix :: String
yellowPrefix = "\ESC[93m"

pinkPrefix :: String
pinkPrefix = "\ESC[95m"

bluePrefix :: String
bluePrefix = "\ESC[94m"

redPrefix :: String
redPrefix = "\ESC[91m"

red :: String -> String
red s = redPrefix ++ s ++ reset

blue :: String -> String
blue s = bluePrefix ++ s ++ reset

reset :: String
reset = "\ESC[0m"

bgBlack :: String -> String
bgBlack s = "\ESC[48;2;10;10;10m" ++ s ++ reset
