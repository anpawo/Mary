{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

module VM.VirtualMachine where

data Value = IntVal Int | BoolVal Bool | FuncVal [Instruction]
  deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Eq | Less
  deriving (Show, Eq)

data AST
  = ASTValue Value
  | ASTBinOp Operator AST AST
  | ASTIf AST AST AST
  | ASTCall String [AST]

data Instruction
  = Push Value
  | Call Operator
  | Ret
  | JumpIfFalse Int
  | PushArg Int
  | PushEnv String
  deriving (Show, Eq)

type Stack = [Value]

type Program = [Instruction]

type Args = [Value]

type Env = [(String, Value)]

type ResolvedEnv = [(Int, Value)]

resolveEnv :: Env -> ResolvedEnv
resolveEnv env = zip [0..] (map snd env)

exec :: ResolvedEnv -> Args -> Program -> Stack -> Either String Value
exec env args [Ret] (x:_) = Right x
exec env args (Push v : is) stack = exec env args is (v : stack)
exec env args (PushArg i : is) stack
  | i < length args = exec env args is (args !! i : stack)
  | otherwise = Left "Invalid argument index"
exec env args (PushEnv name : is) stack =
  case lookup name env of
    Just v  -> exec env args is (v : stack)
    Nothing -> Left $ "Variable " ++ name ++ " not found"
exec env args (Call op : is) stack = case op of
  Add  -> exec env args (Call Add : is) stack
  Sub  -> exec env args (Call Sub : is) stack
  Mul  -> exec env args (Call Mul : is) stack
  Div  -> exec env args (Call Div : is) stack
  Eq   -> exec env args (Call Eq : is) stack
  Less -> exec env args (Call Less : is) stack
exec env args (Call _ : is) (IntVal i : stack)
  | i < length env = case snd (env !! i) of
      FuncVal body -> case exec env [] body stack of
        Right result -> exec env args is (result : stack)
        Left err     -> Left err
      _ -> Left $ "Value at index " ++ show i ++ " is not a function"
  | otherwise = Left $ "Invalid function index: " ++ show i
exec env args (Call Add : is) (IntVal a : IntVal b : stack) = exec env args is (IntVal (b + a) : stack)
exec env args (Call Sub : is) (IntVal a : IntVal b : stack) = exec env args is (IntVal (b - a) : stack)
exec env args (Call Mul : is) (IntVal a : IntVal b : stack) = exec env args is (IntVal (b * a) : stack)
exec env args (Call Div : is) (IntVal a : IntVal b : stack)
  | a /= 0    = exec env args is (IntVal (b `div` a) : stack)
  | otherwise = Left "Division by zero"
exec env args (Call Eq : is) (IntVal a : IntVal b : stack) = exec env args is (BoolVal (a == b) : stack)
exec env args (Call Less : is) (IntVal a : IntVal b : stack) = exec env args is (BoolVal (b < a) : stack)
exec env args (JumpIfFalse n : is) (BoolVal False : stack) = exec env args (drop n is) stack
exec env args (JumpIfFalse _ : is) (_ : stack) = exec env args is stack
exec env args (JumpIfFalse _ : _) (x : _)
  | not (isBoolVal x) = Left "JumpIfFalse expects a boolean on the stack"
  where isBoolVal (BoolVal _) = True
        isBoolVal _           = False
exec _ _ _ _ = Left "Invalid program"

compile :: AST -> Program
compile (ASTValue v) = [Push v]
compile (ASTBinOp op left right) =
  compile left ++ compile right ++ [Call op]
compile (ASTIf cond thenBranch elseBranch) =
  let condCode = compile cond
      thenCode = compile thenBranch
      elseCode = compile elseBranch
  in condCode ++ [JumpIfFalse (length thenCode + 1)] ++ thenCode ++ [JumpIfFalse (length elseCode)] ++ elseCode
compile (ASTCall name args) =
  concatMap compile args ++ [PushEnv name, Call name]