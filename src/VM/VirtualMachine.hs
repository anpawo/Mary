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

exec :: Program -> Stack -> Either String Value
exec [Ret] (x:_) = Right x
exec (Push v : is) stack = exec is (v : stack)
exec (Call Add : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b + a) : stack)
exec (Call Sub : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b - a) : stack)
exec (Call Mul : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b * a) : stack)
exec (Call Div : is) (IntVal a : IntVal b : stack)
  | a /= 0    = exec is (IntVal (b `div` a) : stack)
  | otherwise = Left "Division by zero"
exec (Call Eq : is) (IntVal a : IntVal b : stack) = exec is (BoolVal (a == b) : stack)
exec (Call Less : is) (IntVal a : IntVal b : stack) = exec is (BoolVal (b < a) : stack)
exec (JumpIfFalse n : is) (BoolVal False : stack) = exec (drop n is) stack
exec (JumpIfFalse _ : is) (_ : stack) = exec is stack
exec _ _ = Left "Invalid program"