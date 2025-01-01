{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

data Value = IntVal Int
  deriving (Show, Eq)

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

type ResolvedEnv = [(Int, Value)]

resolveEnv :: Env -> ResolvedEnv
resolveEnv env = zip [0..] (map snd env)

exec :: ResolvedEnv -> Args -> Program -> Stack -> Either String Value
exec env args [Ret] (x:_) = Right x
exec env args (Push v : is) stack = exec env args is (v : stack)
exec env args (PushArg i : is) stack
  | i < length args = exec env args is (args !! i : stack)
  | otherwise = Left "Invalid argument index"
exec env args (PushEnv i : is) stack
  | i < length env = exec env args is ((snd $ env !! i) : stack)
  | otherwise = Left $ "Variable " ++ show i ++ " not found"
exec env args (Call _ : is) (FuncVal body : stack) =
  case exec env [] body stack of
    Right result -> exec env args is (result : stack)
    Left err     -> Left err
exec env args (Call _ : is) (IntVal a : stack) =
  case lookup (show a) env of
    Just (FuncVal body) -> case exec env [] body stack of
      Right result -> exec env args is (result : stack)
      Left err     -> Left err
    _ -> Left $ "Function " ++ show a ++ " not found"
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
exec _ _ _ _ = Left "Invalid program"