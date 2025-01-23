{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module defines the virtual machine for executing instructions.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module VM.VirtualMachine
  (
    Stack,
    Insts,
    Env,
    exec,
    convArrInstrToVal,
    convStructInstrToVal,
    countParamFunc,
    jumpIfFalseInstr,
    callInstr,
    pushInstr,
    doCurrentInstr,
    getcurrentInstr,
  )
where

import Bytecode.Data
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.List (find)
import System.IO (stderr, hPrint, stdout, hFlush)
import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import System.Random (randomRIO)

type Stack = [Value]
type Insts = [Instruction]
type Env = [(String, Insts)]

convArrInstrToVal :: [[Instruction]] -> Env -> IO [Value]
convArrInstrToVal [] _ = pure []
convArrInstrToVal (v:rest) env =
    exec 0 env v [] >>= \value ->
    convArrInstrToVal rest env >>= \restValues ->
    pure (value : restValues)

convStructInstrToVal :: Env -> Env -> IO [(String, Value)]
convStructInstrToVal [] _ = pure []
convStructInstrToVal ((name, v):rest) env =
    exec 0 env v [] >>= \value ->
    convStructInstrToVal rest env >>= \restValues ->
    pure ((name, value) : restValues)

countParamFunc :: [Instruction] -> Int -> Int
countParamFunc [] nb = nb
countParamFunc (Store _: rest) nb = countParamFunc rest (nb + 1)
countParamFunc (_: rest) nb       = countParamFunc rest nb

jumpIfFalseInstr :: Instruction -> Int -> Env -> Insts -> Stack -> IO Value
jumpIfFalseInstr (JumpIfFalse n) ind env is (VmBool False : stack) = exec (ind + n + 1) env is stack
jumpIfFalseInstr (JumpIfFalse _) ind env is (VmBool True : stack) = exec (ind + 1) env is stack
jumpIfFalseInstr (JumpIfFalse _) ind env is (_ : _) = fail "JumpIfFalse expects a boolean on the stack"

callInstr :: String -> Int -> Env -> Insts -> Stack -> IO Value
callInstr name idx env insts stack = case lookup name env of
    Just [Push l@(VmLambda {..})] -> exec 0 (lbEnv ++ env) lbBody stack >>= \v -> exec (idx + 1) env insts (v: drop (countParamFunc lbBody 0) stack) -- lambda
    Just [Push (VmClosure closureName)] -> callInstr closureName idx env insts stack -- consume closure
    Just body -> exec 0 env body stack >>= \res -> exec (idx + 1) env insts (res : drop (countParamFunc body 0) stack)
    Nothing -> builtinOperator name idx env insts stack

pushInstr :: Value -> Int -> Env -> Insts -> Stack -> IO Value
pushInstr (VmPreStruct structName fields) ind env is stack = convStructInstrToVal fields env >>= \values -> exec (ind + 1) env is (VmStruct structName values : stack)
pushInstr (VmPreArray typeName arr) ind env is stack = convArrInstrToVal arr env >>= \values -> exec (ind + 1) env is (VmArray typeName values : stack)
pushInstr (VmPreLambda {..}) ind env is stack = exec (ind + 1) env is (VmLambda (map (\n -> (n, fromJust $ lookup n env)) lbVarCaptured) lbBody :stack)
pushInstr v ind env is stack = exec (ind + 1) env is (v : stack)

doCurrentInstr :: Maybe Instruction -> Int -> Env -> Insts -> Stack -> IO Value
doCurrentInstr (Just Ret) ind _ is (x : _) = pure x
doCurrentInstr (Just Ret) ind _ is [] = fail "Ret expects at least one value on the stack"
doCurrentInstr (Just (Push v)) ind env is stack = pushInstr v ind env is stack
doCurrentInstr (Just (Update name)) ind env is (v : VmString field: stack) = case find ((== name) . fst) env of
  Just (_, x) -> case x of
    [Push (VmStruct name' value)] -> case find ((== field) . fst) value of
      Just (fieldName, currValue) -> exec (ind + 1) ((name, [Push (VmStruct name' (map (\(n, currV) -> if n == field then (n, v) else (n, currV)) value))]) : env) is stack
      Nothing -> fail $ printf "Structure '%s' doesn't have the field '%s'." name' field
    _ -> fail (printf "'%s' is not a structure." name)
  Nothing -> fail (printf "'%s' is not bound" name)
doCurrentInstr (Just (Store name)) ind env is (v : stack) = exec (ind + 1) ((name, [Push v]) : env) is stack
doCurrentInstr (Just (Load name)) ind env is stack = case lookup name env of
  Just body -> exec 0 env body stack >>= \res -> exec (ind + 1) env is (res : drop (countParamFunc body 0) stack)
  Nothing -> fail (printf "'%s' is not bound" name)
doCurrentInstr (Just Call) ind env is (VmFunc name: stack) = callInstr name ind env is stack
doCurrentInstr (Just TailCall) ind env is (VmFunc name: stack) = callInstr name ind env (is ++ [Ret]) stack
doCurrentInstr (Just (JumpIfFalse n)) ind env is stack = jumpIfFalseInstr (JumpIfFalse n) ind env is stack
doCurrentInstr (Just (JumpBackward n)) ind env is stack = exec (ind - n) env is stack
doCurrentInstr Nothing ind _ is (x : _) = pure x
doCurrentInstr Nothing ind _ is [] = pure VmVoid

getcurrentInstr :: Int -> Insts -> Maybe Instruction
getcurrentInstr ind is = if ind < length is then Just (is !! ind) else Nothing

exec :: Int -> Env -> Insts -> Stack -> IO Value
exec idx env insts = doCurrentInstr (getcurrentInstr idx insts) idx env insts

builtinOperator :: String -> Int -> Env -> Insts -> Stack -> IO Value
-- io
builtinOperator "random" ind env is (VmInt upper: VmInt lower : stack) = randomRIO(lower, upper) >>= \x -> exec (ind + 1) env is (VmInt x:stack)
builtinOperator "put" ind env is (v : stack) = putStr (show v) >> hFlush stdout >> exec (ind + 1) env is stack
builtinOperator "print" ind env is (v : stack) = print v >> exec (ind + 1) env is stack
builtinOperator "eprint" ind env is (v : stack) = hPrint stderr v >> exec (ind + 1) env is stack
builtinOperator "getline" ind env is stack = getLine >>= \line -> exec (ind + 1) env is (VmString line:stack)
builtinOperator "exit" ind env is (VmInt n:stack) = if n == 0 then exitSuccess else exitWith $ ExitFailure n
-- struct
builtinOperator "." ind env is (VmString fieldName : VmStruct name fields : rest) = case lookup fieldName fields of
  Just x -> exec (ind + 1) env is (x : rest)
  Nothing -> fail $ printf "Cannot access field `%s` of struct `%s`." fieldName name
builtinOperator "." ind env is (VmString fieldName : VmString name : rest) = case lookup name env of
    Just [Push (VmStruct _ fields)] -> case lookup fieldName fields of
      Just val -> exec (ind + 1) env is (val : rest)
    _ -> fail ". expected a structure to access value"
builtinOperator "set" ind env is (v : VmString field : VmStruct name fields : stack) = case find ((== field) . fst) fields of
  Nothing -> fail $ printf "Cannot access field `%s` of struct `%s`." field name
  _ -> exec (ind + 1) env is (VmStruct name (map (\(n, v') -> if n == field then (n, v) else (n, v')) fields):stack)
-- conversion
builtinOperator "toString" ind env is (v : stack) = exec (ind + 1) env is (VmString (show v):stack)
builtinOperator "toFloat"  ind env is (x:stack) = either fail (\v -> exec (ind + 1) env is (v:stack)) (conversionOperator ToFloat x)
builtinOperator "toInt"    ind env is (x:stack) = either fail (\v -> exec (ind + 1) env is (v:stack)) (conversionOperator ToInt x)
builtinOperator "toChar"   ind env is (x:stack) = either fail (\v -> exec (ind + 1) env is (v:stack)) (conversionOperator ToChar x)
-- misc
builtinOperator "length" ind env is (VmString str : stack) = exec (ind + 1) env is (VmInt (length str):stack)
builtinOperator "length" ind env is (VmArray _ arr : stack) = exec (ind + 1) env is (VmInt (length arr):stack)
builtinOperator "insert" ind env is (VmChar c : VmInt i :VmString str : stack) = exec (ind + 1) env is (VmString (take i str ++ [c] ++ drop i str):stack)
builtinOperator "insert" ind env is (x : VmInt i :VmArray typeName arr : stack) = exec (ind + 1) env is (VmArray typeName (take i arr ++ [x] ++ drop i arr):stack) -- type is not checked
builtinOperator "append" ind env is (VmChar c : VmString str : stack) = exec (ind + 1) env is (VmString (str ++ [c]):stack)
builtinOperator "append" ind env is (x : VmArray typeName arr : stack) = exec (ind + 1) env is (VmArray typeName (arr ++ [x]):stack) -- type is not checked
builtinOperator "at" ind env is (VmInt i : VmString str : stack) = exec (ind + 1) env is (VmChar (str !! i):stack) -- bounds not checked
builtinOperator "at" ind env is (VmInt i : VmArray typeName arr : stack) = exec (ind + 1) env is (arr !! i:stack) -- bounds not checked
builtinOperator "concat" ind env is (VmString str : VmString str' : stack) = exec (ind + 1) env is (VmString (str' ++ str):stack)
builtinOperator "concat" ind env is (VmArray t arr : VmArray t' arr' : stack) = if t == t' then exec (ind + 1) env is (VmArray t (arr' ++ arr):stack) else fail "Cannot concat two arrays of different type"
builtinOperator name ind env insts (a:b:stack) = either fail (\v -> exec (ind + 1) env insts (v:stack)) (arithmeticOperator name b a)
builtinOperator name _ _ _ _ = fail $ printf "Invalid operands for operator '%s'" name


data Conversion = ToInt | ToFloat | ToChar deriving (Show)

conversionOperator :: Conversion -> Value -> Either String Value
-- toChar
conversionOperator ToChar (VmChar c) = Right $ VmChar c
conversionOperator ToChar (VmInt c)  = Right $ VmChar $ chr c

-- toInt
conversionOperator ToInt (VmChar c)   = Right $ VmInt $ ord c
conversionOperator ToInt (VmBool v)   = Right $ VmInt $ fromEnum v
conversionOperator ToInt (VmInt v)    = Right $ VmInt v
conversionOperator ToInt (VmFloat v)  = Right $ VmInt $ floor v
conversionOperator ToInt (VmString v) = maybe (Left "Invalid string for toInt") (Right . VmInt) (readMaybe v)

-- toFloat
conversionOperator ToFloat (VmBool v)   = Right $ VmFloat $ fromIntegral $ fromEnum v
conversionOperator ToFloat (VmFloat v)  = Right $ VmFloat v
conversionOperator ToFloat (VmInt v)    = Right $ VmFloat $ fromIntegral v
conversionOperator ToFloat (VmString v) = maybe (Left "Invalid string for toFloat") (Right . VmFloat) (readMaybe v)

-- error
conversionOperator name _ = Left $ printf "Invalid value for '%s'" $ show name


arithmeticOperator :: String -> Value -> Value -> Either String Value
-- comparison
arithmeticOperator "==" l r = Right $ VmBool $ l == r
arithmeticOperator "is" v (VmString t) = Right $ VmBool (typeCheck v t)
-- int int
arithmeticOperator "+" (VmInt l) (VmInt r) = Right $ VmInt $ l + r
arithmeticOperator "-" (VmInt l) (VmInt r) = Right $ VmInt $ l - r
arithmeticOperator "*" (VmInt l) (VmInt r) = Right $ VmInt $ l * r
arithmeticOperator "/" (VmInt l) (VmInt 0) = Left "Division by zero"
arithmeticOperator "/" (VmInt l) (VmInt r) = Right $ VmInt $ l `div` r
arithmeticOperator "<" (VmInt l) (VmInt r) = Right $ VmBool $ l < r

-- float float
arithmeticOperator "+" (VmFloat l) (VmFloat r) = Right $ VmFloat $ l + r
arithmeticOperator "-" (VmFloat l) (VmFloat r) = Right $ VmFloat $ l - r
arithmeticOperator "*" (VmFloat l) (VmFloat r) = Right $ VmFloat $ l * r
arithmeticOperator "/" (VmFloat l) (VmFloat 0) = Left "Division by zero"
arithmeticOperator "/" (VmFloat l) (VmFloat r) = Right $ VmFloat $ l / r
arithmeticOperator "<" (VmFloat l) (VmFloat r) = Right $ VmBool $ l < r

-- int float
arithmeticOperator "+" (VmInt l) (VmFloat r) = Right $ VmFloat $ fromIntegral l + r
arithmeticOperator "-" (VmInt l) (VmFloat r) = Right $ VmFloat $ fromIntegral l - r
arithmeticOperator "*" (VmInt l) (VmFloat r) = Right $ VmFloat $ fromIntegral l * r
arithmeticOperator "/" (VmInt l) (VmFloat 0) = Left "Division by zero"
arithmeticOperator "/" (VmInt l) (VmFloat r) = Right $ VmFloat $ fromIntegral l / r
arithmeticOperator "<" (VmInt l) (VmFloat r) = Right $ VmBool $ fromIntegral l < r

-- float int
arithmeticOperator "+" (VmFloat l) (VmInt r) = Right $ VmFloat $ l + fromIntegral r
arithmeticOperator "-" (VmFloat l) (VmInt r) = Right $ VmFloat $ l - fromIntegral r
arithmeticOperator "*" (VmFloat l) (VmInt r) = Right $ VmFloat $ l * fromIntegral r
arithmeticOperator "/" (VmFloat l) (VmInt 0) = Left "Division by zero"
arithmeticOperator "/" (VmFloat l) (VmInt r) = Right $ VmFloat $ l / fromIntegral r
arithmeticOperator "<" (VmFloat l) (VmInt r) = Right $ VmBool $ l < fromIntegral r

-- string
arithmeticOperator "+" (VmString l) (VmString r) = Right $ VmString $ l ++ r
arithmeticOperator "*" (VmString l) (VmInt r)    = Right $ VmString $ concat $ replicate r l

-- error
arithmeticOperator op _ _ = Left $ printf "Invalid operands for operator '%s'" op