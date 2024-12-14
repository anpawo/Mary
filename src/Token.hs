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
    FnKw --       \| fn
  | InfixKw --    \| infix
  | StructKw --   \| struct
  | IsKw --       \| is
  | ImportKw --   \| import
  | AsKw --       \| as
  | Symbol
  | CurlyOpen --  \|  {   -> struct definition
  | CurlyClose -- \|  }   -> struct definition
  | ParenOpen --  \|  (   -> resolve expressions
  | ParenClose -- \|  )   -> resolve expressions
  | Assign --     \|  =   -> assign expression to a name (can be a func or a var)
  | Scope --      \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --  \|  ;   -> end of statement
  
  -- Literal
  | IntLit Int --       \| 2
  | FloatLit Double --  \| 1.5
  | StringLit String -- \| "yo"
  
  -- Identifier
  | PrefixId String --  \| factorial, add_2
  | InfixId String  --  \| <*>, +

  -- Type ? (int, float, string, char, number ?)

  deriving (Show, Eq)
