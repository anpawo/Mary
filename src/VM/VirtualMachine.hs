{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module VM.VirtualMachine
  (
    Stack,
    Program,
    Env,
    exec,
    -- , compile
  )
where

import Bytecode.Data
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import Text.Read (readMaybe)
import Text.Printf (printf)

type Stack = [Value]

type Program = [Instruction]

type Env = [EnvVar]

convArrInstrToVal :: [[Instruction]] -> Env -> IO [Value]
convArrInstrToVal [] _ = pure []
convArrInstrToVal (v:rest) env =
    exec 0 env v [] >>= \value ->
    convArrInstrToVal rest env >>= \restValues ->
    pure (value : restValues)

-- handleDisplayStruct :: String -> [(String, [Instruction])] -> Env -> IO String
-- handleDisplayStruct structName fields env = printf "%s{%s}" structName . intercalate ", " . map show <$> getFields fields
--   where
--     getFields [] = pure []
--     getFields ((_, x): xs) =
--       exec 0 env x [] >>= \v ->
--       getFields xs >>= \vs ->
--       pure (v : vs)

-- computeStruct :: String -> [(String, [Instruction])] -> Env -> IO Value
-- computeStruct structName fields env = VmStruct structName <$> computeFields fields
--   where
--     computeFields [] = pure []
--     computeFields ((fieldName, x):xs) =
--       exec 0 env x [] >>= \v ->
--         computeFields xs >>= \vs ->
--           pure ((fieldName, [Push v]) : vs)

countParamFunc :: [Instruction] -> Int -> Int
countParamFunc [] nb = nb
countParamFunc (Store _: rest) nb = countParamFunc rest (nb + 1)
countParamFunc (_: rest) nb = nb

jumpIfFalseInstr :: Instruction -> Int -> Env -> Program -> Stack -> IO Value
jumpIfFalseInstr (JumpIfFalse n) ind env is (VmBool False : stack) = exec (ind + n + 1) env is stack
jumpIfFalseInstr (JumpIfFalse _) ind env is (VmBool True : stack) = exec (ind + 1) env is stack
jumpIfFalseInstr (JumpIfFalse _) ind env is (_ : _) = fail "JumpIfFalse expects a boolean on the stack"

exitCallFunc :: Int -> Env -> Program -> Stack -> IO Value
exitCallFunc ind env is (VmInt 0:stack) = exitSuccess
exitCallFunc ind env is (VmInt status:stack) = exitWith $ ExitFailure status
exitCallFunc ind env is _ = fail "error with exit func"

toIntCallFunc :: Int -> Env -> Program -> Stack -> IO Value
toIntCallFunc ind env is (VmBool v:stack) = exec (ind + 1) env is (VmInt (fromEnum v):stack)
toIntCallFunc ind env is (VmInt v:stack) = exec (ind + 1) env is (VmInt v:stack)
toIntCallFunc ind env is (VmFloat v:stack) = exec (ind + 1) env is (VmInt (floor v):stack)
toIntCallFunc ind env is (VmString v:stack) = case readMaybe v of
  Just i -> exec (ind + 1) env is (VmInt i:stack)
  Nothing -> exec (ind + 1) env is (VmNull:stack)
toIntCallFunc ind env is _ = fail "error with toInt func"

toFloatCallFunc :: Int -> Env -> Program -> Stack -> IO Value
toFloatCallFunc ind env is (VmBool v:stack) = exec (ind + 1) env is (VmFloat (fromIntegral $ fromEnum v):stack)
toFloatCallFunc ind env is (VmFloat v:stack) = exec (ind + 1) env is (VmFloat v:stack)
toFloatCallFunc ind env is (VmInt v:stack) = exec (ind + 1) env is (VmFloat (fromIntegral v):stack)
toFloatCallFunc ind env is (VmString v:stack) = case readMaybe v of
  Just f -> exec (ind + 1) env is (VmFloat f:stack)
  Nothing -> exec (ind + 1) env is (VmNull:stack)
toFloatCallFunc ind env is _ = fail "error with toInt func"

operatorCallFunc :: String -> Int -> Env -> Program -> Stack -> IO Value
operatorCallFunc "+" ind env is stack = operatorExec "+" (+) ind env is stack
operatorCallFunc "-" ind env is stack = operatorExec "-" (-) ind env is stack
operatorCallFunc "*" ind env is stack = operatorExec "*" (*) ind env is stack
operatorCallFunc "/" ind env is stack = operatorExec "/" div ind env is stack
operatorCallFunc "<" ind env is stack = boolOperatorExec "<" (<) ind env is stack
operatorCallFunc "==" ind env is (VmInt a : VmInt b : rest) = exec (ind + 1) env is (VmBool (b == a) : rest)
operatorCallFunc "==" ind env is _ = fail "Eq expects two VmInt on the stack"
operatorCallFunc name ind env is _ = fail "Call expects an operator or a function on top of the stack"

callInstr :: String -> Int -> Env -> Program -> Stack -> IO Value
callInstr "is" ind env is (VmString t : v : stack) = exec (ind + 1) env is (VmBool (typeCheck v t):stack)
callInstr "is" ind env is stack = putStrLn (printf "stack: %s\nind: %s\nenv: %s\ninsts: %s\n" (show stack) (show ind) (show env) (show is) :: String) >> exec (ind + 1) env is stack
-- callInstr "print" ind env is ((VmStruct name fields) : stack) = handleDisplayStruct name fields env >>= putStrLn >> exec (ind + 1) env is stack
callInstr "print" ind env is (v : stack) = print v >> exec (ind + 1) env is stack
callInstr "getline" ind env is stack = getLine >>= \line -> exec (ind + 1) env is (VmString line:stack)
callInstr "exit" ind env is stack = exitCallFunc ind env is stack
callInstr "toInt" ind env is stack = toIntCallFunc ind env is stack
callInstr "toFloat" ind env is stack = toFloatCallFunc ind env is stack
callInstr name ind env is stack = case lookup name env of
    Just body -> exec 0 env body stack >>= \res -> exec (ind + 1) env is (res : drop (countParamFunc body 0) stack)
    Nothing -> operatorCallFunc name ind env is stack

doCurrentInstr :: Maybe Instruction -> Int -> Env -> Program -> Stack -> IO Value
doCurrentInstr (Just Ret) ind _ is (x : _) = pure x
doCurrentInstr (Just Ret) ind _ is [] = fail "Ret expects at least one value on the stack"
-- doCurrentInstr (Just (Push (VmStruct name fields))) ind env is stack = computeStruct name fields env >>= \struct -> exec (ind + 1) env is (struct : stack)
doCurrentInstr (Just (Push (VmPreArray typeName arr))) ind env is stack = convArrInstrToVal arr env >>= \values -> exec (ind + 1) env is ((VmArray typeName values) : stack)
doCurrentInstr (Just (Push v)) ind env is stack = exec (ind + 1) env is (v : stack)
doCurrentInstr (Just (Store name)) ind env is (v : stack) = exec (ind + 1) ((name, [Push v]) : env) is stack
doCurrentInstr (Just (Load name)) ind env is stack = case lookup name env of
  Just body -> exec 0 env body stack >>= \res -> exec (ind + 1) env is (res : drop (countParamFunc body 0) stack)
  Nothing -> fail ("Variable or function " ++ name ++ " not found")
doCurrentInstr (Just Call) ind env is (VmFunc name: stack) = callInstr name ind env is stack
doCurrentInstr (Just (JumpIfFalse n)) ind env is stack = jumpIfFalseInstr (JumpIfFalse n) ind env is stack
doCurrentInstr (Just (JumpBackward n)) ind env is stack = exec (ind - n) env is stack
doCurrentInstr Nothing ind _ is (x : _) = pure x
doCurrentInstr Nothing ind _ is [] = pure VmVoid

getcurrentInstr :: Int -> Program -> Maybe Instruction
getcurrentInstr ind is = if ind < length is then Just (is !! ind) else Nothing

exec :: Int -> Env -> Program -> Stack -> IO Value
exec ind env is = doCurrentInstr (getcurrentInstr ind is) ind env is

operatorExec :: String -> (Int -> Int -> Int) -> Int -> Env -> Program -> Stack -> IO Value
operatorExec "/" _ _ _ _(VmInt 0 : _) =  fail "Division by 0 is prohibited"
operatorExec "%" _ _ _ _(VmInt 0 : _) =  fail "Division by 0 is prohibited"
operatorExec _ func ind env is (VmInt a : VmInt b : rest) = exec (ind + 1) env is (VmInt (func b a) : rest)
operatorExec name _ _ _ _ _ = fail $ name ++ " expects two VmInt on the stack"

boolOperatorExec :: String -> (Int -> Int -> Bool) -> Int -> Env -> Program -> Stack -> IO Value
boolOperatorExec _ func ind env is (VmInt a : VmInt b : rest) = exec (ind + 1) env is (VmBool (func b a) : rest)
boolOperatorExec name _ _ _ _ _ = fail $ name ++ " expects two VmInt on the stack"
