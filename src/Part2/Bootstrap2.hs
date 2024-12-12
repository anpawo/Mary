{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- bs2
-}

module Part2.Bootstrap2 (Val (..), exec) where

import Part2.ErrMessage

data Val
  = TInt Int
  | TBool Bool
  | TVoid
  deriving (Show, Eq)

data Builtin
  = BAdd
  | BSub
  | BMul
  | BDiv
  | BEq
  | BLt
  | BGt
  deriving (Show, Eq)

data Instruction
  = IPush Val
  | IPushArg Int
  | ICall Builtin
  | IJumpE Int
  | IJumpNE Int
  | IRet
  deriving (Show, Eq)

type Stack = [Val]

type Args = [Val]

type Insts = [Instruction]

type ExecErr = String

exec :: Args -> Insts -> Stack -> Either ExecErr Val
exec _ [] _ = Right TVoid
-- ret
exec _ (IRet : _) [] = Left $ errArgNb "ret" 1
exec _ (IRet : _) (v : _) = Right v
-- push
exec args (IPush (TBool x) : iRst) stack = exec args iRst (TInt (fromEnum x) : stack)
exec args (IPush x : iRst) stack = exec args iRst (x : stack)
-- push arg
exec args (IPushArg index : iRst) stack = exec args iRst (args !! index : stack)
-- call
exec args (ICall i : iRst) stack = execI i stack >>= exec args iRst
-- jump
exec args (IJumpE n : iRst) (TInt cond : stack)
  | cond == 1 = exec args iRst (drop n stack)
  | otherwise = exec args iRst stack
exec _ (IJumpE _ : _) (_ : _) = Left $ errType "jump" "int"
exec _ (IJumpE _ : _) _ = Left errJump
exec args (IJumpNE n : iRst) (TInt cond : stack)
  | cond == 0 = exec args iRst (drop n stack)
  | otherwise = exec args iRst stack
exec _ (IJumpNE _ : _) (_ : _) = Left $ errType "jump" "int"
exec _ (IJumpNE _ : _) _ = Left errJump

execI :: Builtin -> Stack -> Either ExecErr Stack
-- add
execI BAdd (TInt l : TInt r : rst) = Right $ TInt (l + r) : rst
execI BAdd (_ : _ : _) = Left $ errType "add" "int"
execI BAdd _ = Left $ errArgNb "add" 2
-- sub
execI BSub (TInt l : TInt r : rst) = Right $ TInt (l - r) : rst
execI BSub (_ : _ : _) = Left $ errType "sub" "int"
execI BSub _ = Left $ errArgNb "sub" 2
-- mul
execI BMul (TInt l : TInt r : rst) = Right $ TInt (l * r) : rst
execI BMul (_ : _ : _) = Left $ errType "mul" "int"
execI BMul _ = Left $ errArgNb "mul" 2
-- div
execI BDiv (TInt _ : TInt 0 : _) = Left "Division by 0"
execI BDiv (TInt l : TInt r : rst) = Right $ TInt (l `div` r) : rst
execI BDiv (_ : _ : _) = Left $ errType "div" "int"
execI BDiv _ = Left $ errArgNb "div" 2
-- eq
execI BEq (l : r : rst) = Right $ TInt (fromEnum (l == r)) : rst
execI BEq _ = Left $ errArgNb "equal" 2
-- lt
execI BLt (TInt l : TInt r : rst) = Right $ TInt (fromEnum (l < r)) : rst
execI BLt (_ : _ : _) = Left $ errType "less than" "int"
execI BLt _ = Left $ errArgNb "less then" 2
-- gt
execI BGt (TInt l : TInt r : rst) = Right $ TInt (fromEnum (l > r)) : rst
execI BGt (_ : _ : _) = Left $ errType "greater than" "int"
execI BGt _ = Left $ errArgNb "greater than" 2
