{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

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

type Stack = [Value]

type Program = [Instruction]

type Env = [EnvVar]

-- exec :: Int -> Env -> Program -> Stack -> Either String Value

-- -- return the value on top of the stack
-- exec ind _ (Ret : _) (x : _) =
--   Right x
-- exec ind _ (Ret : _) [] =
--   Left "Ret expects at least one value on the stack"

-- -- push the value on the stack
-- exec ind env (Push v : is) stack =
--   exec (ind + 1) env is (v : stack)

-- -- Store the value on top of the stack in the environment
-- exec ind env (Store name : is) (v : stack) =
--   exec (ind + 1) ((name, [Push v]) : env) is stack

-- -- Load the value from the environment and push it on the stack
-- exec ind env (Load name : is) stack = case lookup name env of
--     Just body  -> case exec 0 env body stack of
--         Right res -> exec (ind + 1) env is (res : stack)
--         Left _ -> Left "Error loading instructions"
--     Nothing -> Left ("Variable or function " ++ name ++ " not found")

-- -- -- call pop the function from the stack and execute it
-- exec ind env (Call : is) (v : stack) = case v of
--     (VmFunc "+") -> operatorExec "+" (+) env is stack
--     (VmFunc "-") -> operatorExec "-" (-) env is stack
--     (VmFunc "*") -> operatorExec "*" (*) env is stack
--     (VmFunc "/") -> operatorExec "/" div env is stack
--     (VmFunc "<") -> boolOperatorExec "<" (<) env is stack
--     (VmFunc "==") -> case stack of
--       (VmInt a : VmInt b : rest) ->
--         exec ind env is (VmBool (b == a) : rest)
--       _ -> Left "Eq expects two VmInt on the stack"
--     VmFunc name -> case lookup name env of
--       Just body  -> case exec 0 env body stack of
--         Right res -> exec (ind + 1) env is (res : stack)
--         Left _ -> Left "Error executing instructions"
--       Nothing -> Left ("Variable or function " ++ name ++ " not found")
--     _ -> Left " Call expects an operator or a function on top of the stack 2345678"

-- -- jump if the value on top of the stack is false
-- exec ind env (JumpIfFalse n : is) (VmBool False : stack) =
--   exec (ind + n) env is stack

-- -- jump if the value on top of the stack is true
-- exec ind env (JumpIfFalse _ : is) (VmBool True : stack) =
--   exec (ind + 1) env is stack

-- -- error case
-- exec ind _ (JumpIfFalse _ : _) (_ : _) =
--   Left "JumpIfFalse expects a boolean on the stack"

-- -- jump backward
-- exec ind env (JumpBackward n : is) stack =
--   exec (ind - n) env is stack -- TODO je sais meme pas comment faire ça, peut etre qu'on peut garder une copie de la pile avant le jump et la remettre après le jump ?

-- exec ind _ [] (x : _) = Right x
-- exec ind _ [] [] = Left "No value in stack at end of program"
-- exec ind _ _ _ = Left "Invalid program"

doCurrentInstr :: Maybe Instruction -> Int -> Env -> Program -> Stack -> Either String Value
doCurrentInstr (Just Ret) ind _ is (x : _) = Right x
doCurrentInstr (Just Ret) ind _ is [] = Left "Ret expects at least one value on the stack"
doCurrentInstr (Just (Push v)) ind env is stack = exec (ind + 1) env is (v : stack)
doCurrentInstr (Just (Store name)) ind env is (v : stack) = exec (ind + 1) ((name, [Push v]) : env) is stack
doCurrentInstr (Just (Load name)) ind env is stack = case lookup name env of
  Just body -> case exec 0 env body stack of
    Right res -> exec (ind + 1) env is (res : stack)
    Left _    -> Left "Error loading instructions"
  Nothing -> Left ("Variable or function " ++ name ++ " not found")
doCurrentInstr (Just Call) ind env is (v : stack) = case v of
  (VmFunc "+") -> operatorExec "+" (+) ind env is stack
  (VmFunc "-") -> operatorExec "-" (-) ind env is stack
  (VmFunc "*") -> operatorExec "*" (*) ind env is stack
  (VmFunc "/") -> operatorExec "/" div ind env is stack
  (VmFunc "<") -> boolOperatorExec "<" (<) ind env is stack
  (VmFunc "==") -> case stack of
    (VmInt a : VmInt b : rest) -> exec (ind + 1) env is (VmBool (b == a) : rest)
    _ -> Left "Eq expects two VmInt on the stack"
  VmFunc name -> case lookup name env of
    Just body -> case exec 0 env body stack of
      Right res -> exec (ind + 1) env is (res : stack)
      Left _    -> Left "Error executing instructions"
    Nothing -> Left ("Variable or function " ++ name ++ " not found")
  _ -> Left "Call expects an operator or a function on top of the stack"
doCurrentInstr (Just (JumpIfFalse n)) ind env is (VmBool False : stack) = exec (ind + n) env is stack
doCurrentInstr (Just (JumpIfFalse _)) ind env is (VmBool True : stack) = exec (ind + 1) env is stack
doCurrentInstr (Just (JumpIfFalse _)) ind env is (_ : _) = Left "JumpIfFalse expects a boolean on the stack"
doCurrentInstr (Just (JumpBackward n)) ind env is stack = exec (ind - n) env is stack
doCurrentInstr Nothing ind _ is (x : _) = Right x
doCurrentInstr Nothing ind _ is [] = Left "No value in stack at end of program"

getcurrentInstr :: Int -> Program -> Maybe Instruction
getcurrentInstr ind is = if ind < length is then Just (is !! ind) else Nothing

exec :: Int -> Env -> Program -> Stack -> Either String Value
exec ind env is stack = doCurrentInstr (getcurrentInstr ind is) ind env is stack

operatorExec :: String -> (Int -> Int -> Int) -> Int -> Env -> Program -> Stack -> Either String Value
operatorExec "/" _ _ _ _(VmInt 0 : _) =  Left "Division by 0 is prohibited"
operatorExec "%" _ _ _ _(VmInt 0 : _) =  Left "Division by 0 is prohibited"
operatorExec _ func ind env is (VmInt a : VmInt b : rest) = exec (ind + 1) env is (VmInt (func b a) : rest)
operatorExec name _ _ _ _ _ = Left $ name ++ " expects two VmInt on the stack"

boolOperatorExec :: String -> (Int -> Int -> Bool) -> Int -> Env -> Program -> Stack -> Either String Value
boolOperatorExec _ func ind env is (VmInt a : VmInt b : rest) = exec (ind + 1) env is (VmBool (func b a) : rest)
boolOperatorExec name _ _ _ _ _ =  Left $ name ++ " expects two VmInt on the stack"
