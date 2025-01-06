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
    Args,
    Env,
    exec,
    -- , compile
  )
where

import Bytecode.Data

type Stack = [Value]

type Program = [Instruction]

type Args = [Value]

type Env = [(String, Value)]

exec :: Env -> Program -> Stack -> Either String Value

-- return the value on top of the stack
exec _ (Ret : _) (x : _) =
  Right x
exec _ (Ret : _) [] =
  Left "Ret expects at least one value on the stack"

-- push the value on the stack
exec env (Push v : is) stack =
  exec env is (v : stack)

-- Store the value on top of the stack in the environment
exec env (Store name : is) (v : stack) =
  exec ((name, v) : env) is stack

-- Load the value from the environment and push it on the stack
exec env (Load name : is) stack =
  case lookup name env of
    Just v -> exec env is (v : stack)
    Nothing -> Left ("Variable or function " ++ name ++ " not found")

-- push the argument value on the stack
-- exec env (PushArg i : is) stack
--   | i < length args = exec env args is (args !! i : stack)
--   | otherwise       = Left "Invalid argument index"

-- push the variable value on the stack
-- exec env (PushEnv name : is) stack =
--   case lookup name env of
--     Just v  -> exec env is (v : stack)
--     Nothing -> Left ("Variable or function " ++ name ++ " not found")

-- -- call pop the function from the stack and execute it
exec env (Call : is) (v : stack) =
  case v of
    VmFunc "+" -> operatorExec "+" (+) env is stack
    VmFunc "-" -> operatorExec "-" (-) env is stack
    VmFunc "*" -> operatorExec "*" (*) env is stack
    VmFunc "/" -> operatorExec "/" div env is stack
    VmFunc "<" -> boolOperatorExec "<" (<) env is stack
    VmFunc "==" -> case stack of
      (VmInt a : VmInt b : rest) ->
        exec env is (VmBool (b == a) : rest)
      _ -> Left "Eq expects two VmInt on the stack"
    -- body -> case stack of
    --   (arg : rest) ->
    --     case exec env body [] of
    --       Right result -> exec env is (result : rest)
    --       Left err     -> Left err
    --   [] ->
    --      Left "Function expects 1 argument, but stack is empty!"
    _ -> Left "Call expects an operator or a function on top of the stack"

-- jump if the value on top of the stack is false
exec env (JumpIfFalse n : is) (VmBool False : stack) =
  exec env (drop n is) stack

-- jump if the value on top of the stack is true
exec env (JumpIfFalse _ : is) (VmBool True : stack) =
  exec env is stack

-- error case
exec _ (JumpIfFalse _ : _) (_ : _) =
  Left "JumpIfFalse expects a boolean on the stack"

-- jump backward
-- exec env (JumpBackward n : is) stack =
--   exec env (drop n is) stack -- TODO je sais meme pas comment faire ça, peut etre qu'on peut garder une copie de la pile avant le jump et la remettre après le jump ?

exec _ [] (x : _) = Right x
exec _ [] [] = Left "No value in stack at end of program"
exec _ _ _ = Left "Invalid program"

operatorExec :: String -> (Int -> Int -> Int) -> Env -> Program -> Stack -> Either String Value
operatorExec "/" _ _ _(VmInt 0 : _) =  Left "Division by 0 is prohibited"
operatorExec "%" _ _ _(VmInt 0 : _) =  Left "Division by 0 is prohibited"
operatorExec _ func env is (VmInt a : VmInt b : rest) = exec env is (VmInt (func b a) : rest)
operatorExec name _ _ _ _ = Left $ name ++ " expects two VmInt on the stack"

boolOperatorExec :: String -> (Int -> Int -> Bool) -> Env -> Program -> Stack -> Either String Value
boolOperatorExec _ func env is (VmInt a : VmInt b : rest) = exec env is (VmBool (func b a) : rest)
boolOperatorExec name  _ _ _ _ =  Left $ name ++ " expects two VmInt on the stack"

-- compile an AST to a program
-- compile :: AST -> Program
-- compile (ASTValue v) =
--   [ Push v ]

-- -- compile a binary operation
-- compile (ASTBinOp op left right) =
--   compile left
--   ++ compile right
--   ++ [ Push (OpVal op)
--      , Call
--      ]
-- -- compile an if statement
-- compile (ASTIf cond thenBranch elseBranch) =
--   let condCode = compile cond
--       thenCode = compile thenBranch
--       elseCode = compile elseBranch
--   in
--     condCode
--     ++ [ JumpIfFalse (length thenCode + 1) ]
--     ++ thenCode
--     ++ [ JumpIfFalse (length elseCode) ]
--     ++ elseCode

-- compile a function call
-- compile (ASTCall name) =
--   concatMap compile
--   ++ [ PushEnv name
--      , Call
--      ]