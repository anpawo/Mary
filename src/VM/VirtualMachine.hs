{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

module VM.VirtualMachine
  (
    Value(..)
  , Operator(..)
  , AST(..)
  , Instruction(..)

  , Stack
  , Program
  , Args
  , Env

  , exec
  , compile
  ) where

data Value
  = VmInt Int
  | VmFloat Double
  | VmBool Bool
  | VmChar Char
  | VmString String
  | VmArray [Instruction]
  | VmStruct [(String, [Instruction])]
  | VmNull
  | OpVal Operator
  | FunVal [Instruction]
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Less
  deriving (Show, Eq)

data AST
  = ASTValue Value
  | ASTBinOp Operator AST AST
  | ASTIf AST AST AST
  | ASTCall String [AST]
  deriving (Show, Eq)

data Instruction
  = Push Value
  | Call
  | Ret
  | JumpIfFalse Int
  | Store String
  | Load String
  | PushEnv String
  | JumpBackward Int
  deriving (Show, Eq)

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
    Just v  -> exec env is (v : stack)
    Nothing -> Left ("Variable or function " ++ name ++ " not found")

-- push the argument value on the stack
-- exec env (PushArg i : is) stack
--   | i < length args = exec env args is (args !! i : stack)
--   | otherwise       = Left "Invalid argument index"

-- push the variable value on the stack
exec env (PushEnv name : is) stack =
  case lookup name env of
    Just v  -> exec env is (v : stack)
    Nothing -> Left ("Variable or function " ++ name ++ " not found")

-- call pop the function from the stack and execute it
exec env (Call : is) (v : stack) =
  case v of
    OpVal Add  -> case stack of
      (VmInt a : VmInt b : rest) ->
         exec env is (VmInt (b + a) : rest)
      _ -> Left "Add expects two VmInt on the stack"
    OpVal Sub  -> case stack of
      (VmInt a : VmInt b : rest) ->
         exec env is (VmInt (b - a) : rest)
      _ -> Left "Sub expects two VmInt on the stack"
    OpVal Mul  -> case stack of
      (VmInt a : VmInt b : rest) ->
         exec env is (VmInt (b * a) : rest)
      _ -> Left "Mul expects two VmInt on the stack"
    OpVal Div  -> case stack of
      (VmInt a : VmInt b : rest) ->
        if a == 0
          then Left "Division by zero"
          else exec env is (VmInt (b `div` a) : rest)
      _ -> Left "Div expects two VmInt on the stack"
    OpVal Eq   -> case stack of
      (VmInt a : VmInt b : rest) ->
        exec env is (VmBool (b == a) : rest)
      _ -> Left "Eq expects two VmInt on the stack"
    OpVal Less -> case stack of
      (VmInt a : VmInt b : rest) ->
        exec env is (VmBool (b < a) : rest)
      _ -> Left "Less expects two VmInt on the stack"
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
exec _  (JumpIfFalse _ : _) (_ : _) =
  Left "JumpIfFalse expects a boolean on the stack" 

-- jump backward
-- exec env (JumpBackward n : is) stack =
--   exec env (drop n is) stack -- TODO je sais meme pas comment faire ça, peut etre qu'on peut garder une copie de la pile avant le jump et la remettre après le jump ?

exec _ [] (x : _) = Right x
exec _ [] []      = Left "No value in stack at end of program"
exec _ _ _        = Left "Invalid program"

-- compile an AST to a program
compile :: AST -> Program
compile (ASTValue v) =
  [ Push v ]

-- compile a binary operation
compile (ASTBinOp op left right) =
  compile left
  ++ compile right
  ++ [ Push (OpVal op)
     , Call
     ]
-- compile an if statement
compile (ASTIf cond thenBranch elseBranch) =
  let condCode = compile cond
      thenCode = compile thenBranch
      elseCode = compile elseBranch
  in
    condCode
    ++ [ JumpIfFalse (length thenCode + 1) ]
    ++ thenCode
    ++ [ JumpIfFalse (length elseCode) ]
    ++ elseCode

-- compile a function call
-- compile (ASTCall name) =
--   concatMap compile
--   ++ [ PushEnv name
--      , Call
--      ]