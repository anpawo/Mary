{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}

module Token (Token (..)) where

-- \| SingleLineComment --    //
-- \| MultiLineCommentOpen --    /*
-- \| MultiLineCommentClose --    */

data Token
  =
  -- Keyword
    FunctionKw -- \| fn
  | InfixKw --    \| infix
  | StructKw --   \| struct
  | ImportKw --   \| import
  | AsKw --       \| as
  | IsKw --       \| is
  | AtKw --       \| at

  -- Symbol
  | CurlyOpen --  \|  {   -> struct definition
  | CurlyClose -- \|  }   -> struct definition
  | Arrow --      \|  ->  -> return type of functions
  | ParenOpen --  \|  (   -> resolve expressions
  | ParenClose -- \|  )   -> resolve expressions
  | Assign --     \|  =   -> assign expression to a name (can be a func or a var)
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

  -- Type ? (int, float, string, char, number ?)

  deriving (Show, Eq)
