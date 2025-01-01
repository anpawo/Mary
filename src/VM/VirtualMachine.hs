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