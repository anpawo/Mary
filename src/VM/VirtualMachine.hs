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

data Operator = Add | Sub | Mul | Div
  deriving (Show, Eq)

data Instruction
  = Push Value
  | Call Operator
  | Ret
  deriving (Show, Eq)

type Stack = [Value]

type Program = [Instruction]

exec :: Program -> Stack -> Value
exec [Ret] (x:_) = x
exec (Push v : is) stack = exec is (v : stack)
exec (Call Add : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b + a) : stack)
exec (Call Sub : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b - a) : stack)
exec (Call Mul : is) (IntVal a : IntVal b : stack) = exec is (IntVal (b * a) : stack)
exec (Call Div : is) (IntVal a : IntVal b : stack)
  | a /= 0    = exec is (IntVal (b `div` a) : stack)
  | otherwise = error "Division by zero"
exec _ _ = error "Invalid program"