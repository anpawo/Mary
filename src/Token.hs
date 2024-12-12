{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}

module Token (TokenType (..), Keyword (..), Symbol (..), Literal (..), test) where

-- \| SingleLineComment --    //
-- \| MultiLineCommentOpen --    /*
-- \| MultiLineCommentClose --    */

data Keyword
  = FnKw --       \| fn
  | StructKw --   \| struct
  | MacroKw --    \| macro
  | IsKw --       \| is
  | ImportKw --   \| import
  | AsKw --       \| as
  deriving (Show, Eq)

data Symbol
  = CurlyOpen --  \|  {   -> struct definition
  | CurlyClose -- \|  }   -> struct definition
  | ParenOpen --  \|  (   -> resolve expressions
  | ParenClose -- \|  )   -> resolve expressions
  | Equal --      \|  =   -> assign expression to a name (can be a func or a var)
  | Scope --      \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --  \|  ;   -> end of statement
  deriving (Show, Eq)

data Literal
  = IntLit --     \| 2
  | FloatLit --   \| 1.5
  | StringLit --  \| "yo"
  deriving (Show, Eq)

data TokenType = Kw Keyword | Symb Symbol | Lit Literal deriving (Show, Eq)

test :: TokenType -> String
test (Lit x) = show x
test (Kw x) = show x
test (Symb x) = show x
