{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser.Token (Token (..), Type (..), Literal(..), Identifier(..)) where

data Type
  = CharType --             \| char
  | VoidType --             \| void
  | BoolType --             \| bool
  | IntType --              \| int
  | FloatType --            \| float
  | StrType --              \| str
  | ArrType --              \| arr
  deriving (Show, Eq, Ord)

data Literal
  = CharLit Char --      \| 'c'   -> may be a list of char
  | BoolLit Bool --      \| true | false
  | IntLit Int --        \| 2
  | FloatLit Double --   \| 1.5
  | StringLit String --  \| "yo"   -> may be a list of char
  deriving (Show, Eq, Ord)

data Identifier
  = SymbolId String --   \| factorial, add_2, x
  | OperatorId String -- \| <*>, +, *, **
  deriving (Show, Eq, Ord)

data Token
  =
  -- Keyword
    FunctionKw --        \| function     -> declare a function
  | OperatorKw --        \| operator     -> declare an operator
  | PrecedenceKw --      \| precedence   -> declare an operator precedence
  | StructKw --          \| struct       -> declare a struct
  | IsKw --              \| is           -> compare types
  | ImportKw --          \| import       -> for imports (bonus)
  | AsKw --              \| as           -> for imports (bonus)
  | AtKw --              \| at           -> get elem from array
  | IfKw --              \| if           -> if
  | ElseKw --            \| else         -> else
  | ReturnKw --          \| return       -> return
  -- Symbol
  | CurlyOpen --         \|  {   -> struct definition
  | CurlyClose --        \|  }   -> struct definition
  | ParenOpen --         \|  (   -> resolve expressions
  | ParenClose --        \|  )   -> resolve expressions
  | BracketOpen --       \|  [   -> array start
  | BracketClose --      \|  ]   -> array end
  | Assign --            \|  =   -> assign expression to a name (can be a func or a var)
  | Arrow --             \|  ->  -> return type of functions
  | Scope --             \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --         \|  ;   -> end of statement
  | Comma --             \|  ,   -> separate arguments in function call/creation
  -- Type
  | Type Type
  -- Literal
  | Literal Literal
  -- Identifier
  | Identifier Identifier
  deriving (Show, Eq, Ord)
