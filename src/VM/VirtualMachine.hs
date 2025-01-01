{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

module VM.VirtualMachine where

data Value
  = IntVal Int
  | BoolVal Bool
  | OpVal Operator
  | FuncVal [Instruction]
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
  | PushArg Int
  | PushEnv String
  deriving (Show, Eq)

type Stack = [Value]
type Program = [Instruction]
type Args = [Value]
type Env = [(String, Value)]

exec :: Env -> Args -> Program -> Stack -> Either String Value

-- return the value on top of the stack
exec _    _    [Ret] (x:_) =
  Right x

-- push the value on the stack
exec env args (Push v : is) stack =
  exec env args is (v : stack)

-- push the argument value on the stack
exec env args (PushArg i : is) stack
  | i < length args = exec env args is (args !! i : stack)
  | otherwise       = Left "Invalid argument index"

-- push the variable value on the stack
exec env args (PushEnv name : is) stack =
  case lookup name env of
    Just v  -> exec env args is (v : stack)
    Nothing -> Left ("Variable " ++ name ++ " not found")

-- call pop the function from the stack and execute it
exec env args (Call : is) (v : stack) =
  case v of
    OpVal Add  -> case stack of
      (IntVal a : IntVal b : rest) ->
         exec env args is (IntVal (b + a) : rest)
      _ -> Left "Add expects two IntVal on the stack"
    OpVal Sub  -> case stack of
      (IntVal a : IntVal b : rest) ->
         exec env args is (IntVal (b - a) : rest)
      _ -> Left "Sub expects two IntVal on the stack"
    OpVal Mul  -> case stack of
      (IntVal a : IntVal b : rest) ->
         exec env args is (IntVal (b * a) : rest)
      _ -> Left "Mul expects two IntVal on the stack"
    OpVal Div  -> case stack of
      (IntVal a : IntVal b : rest) ->
        if a == 0
          then Left "Division by zero"
          else exec env args is (IntVal (b `div` a) : rest)
      _ -> Left "Div expects two IntVal on the stack"
    OpVal Eq   -> case stack of
      (IntVal a : IntVal b : rest) ->
        exec env args is (BoolVal (b == a) : rest)
      _ -> Left "Eq expects two IntVal on the stack"
    OpVal Less -> case stack of
      (IntVal a : IntVal b : rest) ->
        exec env args is (BoolVal (b < a) : rest)
      _ -> Left "Less expects two IntVal on the stack"
    FuncVal body ->
      case exec env [] body stack of
        Right result -> exec env args is (result : stack)
        Left err     -> Left err
    _ -> Left "Call expects an operator or a function on top of the stack"
exec env args (JumpIfFalse n : is) (BoolVal False : stack) = exec env args (drop n is) stack
exec env args (JumpIfFalse _ : is) (_ : stack) = exec env args is stack
exec _   _    (JumpIfFalse _ : _) (x : _)
  | not (isBoolVal x) = Left "JumpIfFalse expects a boolean on the stack"
  where isBoolVal (BoolVal _) = True
        isBoolVal _           = False
exec _ _ _ _ = Left "Invalid program"

compile :: AST -> Program
compile (ASTValue v) =
  [ Push v ]

compile (ASTBinOp op left right) =
  compile left
  ++ compile right
  ++ [ Push (OpVal op)
     , Call
     ]

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

compile (ASTCall name args) =
  concatMap compile args
  ++ [ PushEnv name
     , Call
     ]