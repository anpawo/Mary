{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}

module Parser.Token (Token (..)) where

data Token
  =
  -- Keyword
    FunctionKw -- \| function
  | InfixKw --    \| infix
  | StructKw --   \| struct
  | IsKw --       \| is
  | ImportKw --   \| import
  | AsKw --       \| as
  -- Symbol
  | CurlyOpen --  \|  {   -> struct definition
  | CurlyClose -- \|  }   -> struct definition
  | ParenOpen --  \|  (   -> resolve expressions
  | ParenClose -- \|  )   -> resolve expressions
  | Assign --     \|  =   -> assign expression to a name (can be a func or a var)
  | Arrow --      \|  ->  -> return type of functions
  | Scope --      \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --  \|  ;   -> end of statement
  -- Type
  | CharT --  \| char
  | BoolT --  \| bool
  | IntT --   \| int
  | FloatT -- \| float
  | StrT --   \| str
  | ArrT --   \| arr
  -- Literal
  | CharLit Char --     \| 'c'   -> may be a list of char
  | BoolLit Bool --     \| true | false
  | IntLit Int --       \| 2
  | FloatLit Double --  \| 1.5
  | StringLit String -- \| "yo"   -> may be a list of char
  -- Identifier
  | SymbolId String --  \| factorial, add_2, x
  | OperatorId String  --  \| <*>, +
  deriving (Show, Eq)
