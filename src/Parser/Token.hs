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
    FunctionKw -- \| function -> declare a function
  | InfixKw --    \| infix    -> declare an operator
  | StructKw --   \| struct   -> declare a struct
  | IsKw --       \| is       -> compare types
  | ImportKw --   \| import   -> for imports (bonus)
  | AsKw --       \| as       -> for imports (bonus)
  | AtKw --       \| at       -> get elem from array
  | IfKw --       \| if       -> if
  | ElseKw --     \| else     -> else
  -- Symbol
  | CurlyOpen --    \|  {   -> struct definition
  | CurlyClose --   \|  }   -> struct definition
  | ParenOpen --    \|  (   -> resolve expressions
  | ParenClose --   \|  )   -> resolve expressions
  | BracketOpen --  \|  [   -> array start
  | BracketClose -- \|  ]   -> array end
  | Assign --       \|  =   -> assign expression to a name (can be a func or a var)
  | Arrow --        \|  ->  -> return type of functions
  | Scope --        \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --    \|  ;   -> end of statement
  | Comma --       \|  ,   -> separate arguments in function call/creation
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
  -- Sort token
  | Line [Token]
  | FunctionSort {funcSortName :: String, funcSortParam:: [Token], funcSortType :: Token, funcSortBody :: [Token]}
  | StructureSort {structSortName :: String, structSortBody :: [Token]}
  deriving (Show, Eq)
