{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual machine
-}

data Value = IntVal Int
  deriving (Show, Eq)

data Value = IntVal Int | BoolVal Bool
  deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Eq | Less
  deriving (Show, Eq)

data Instruction
  = Push Value
  | Call Operator
  | Ret
  | JumpIfFalse Int
  | PushArg Int
  deriving (Show, Eq)

type Stack = [Value]

type Program = [Instruction]

type Args = [Value]

type Env = [(String, Value)]

exec :: Args -> Program -> Stack -> Either String Value
exec args [Ret] (x:_) = Right x
exec args (Push v : is) stack = exec args is (v : stack)
exec args (PushArg i : is) stack
  | i < length args = exec args is (args !! i : stack)
  | otherwise = Left "Invalid argument index"
exec args (Call Add : is) (IntVal a : IntVal b : stack) = exec args is (IntVal (b + a) : stack)
exec args (Call Sub : is) (IntVal a : IntVal b : stack) = exec args is (IntVal (b - a) : stack)
exec args (Call Mul : is) (IntVal a : IntVal b : stack) = exec args is (IntVal (b * a) : stack)
exec args (Call Div : is) (IntVal a : IntVal b : stack)
  | a /= 0    = exec args is (IntVal (b `div` a) : stack)
  | otherwise = Left "Division by zero"
exec args (Call Eq : is) (IntVal a : IntVal b : stack) = exec args is (BoolVal (a == b) : stack)
exec args (Call Less : is) (IntVal a : IntVal b : stack) = exec args is (BoolVal (b < a) : stack)
exec args (JumpIfFalse n : is) (BoolVal False : stack) = exec args (drop n is) stack
exec args (JumpIfFalse _ : is) (_ : stack) = exec args is stack
exec _ _ _ = Left "Invalid program"