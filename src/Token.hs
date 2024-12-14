{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Token
-}

module Token (TokenType (..), Keyword (..), Symbol (..), Literal (..), Identifier (..)) where

-- \| SingleLineComment --    //
-- \| MultiLineCommentOpen --    /*
-- \| MultiLineCommentClose --    */

data Keyword
  = FnKw --       \| fn
  | InfixKw --    \| infix
  | StructKw --   \| struct
  | IsKw --       \| is
  | ImportKw --   \| import
  | AsKw --       \| as
  deriving (Show, Eq)

data Symbol
  = CurlyOpen --  \|  {   -> struct definition
  | CurlyClose -- \|  }   -> struct definition
  | ParenOpen --  \|  (   -> resolve expressions
  | ParenClose -- \|  )   -> resolve expressions
  | Assign --     \|  =   -> assign expression to a name (can be a func or a var)
  | Scope --      \|  .   -> used to import a module (std.print, math.facto, ...)
  | SemiColon --  \|  ;   -> end of statement
  deriving (Show, Eq)

data Literal
  = IntLit Int --       \| 2
  | FloatLit Double --  \| 1.5
  | StringLit String -- \| "yo"
  deriving (Show, Eq)

data Identifier
  = PrefixId String --  \| factorial, add_2
  | InfixId String  --  \| <*>, +
  deriving (Show, Eq)

data TokenType = Kw Keyword | Sym Symbol | Lit Literal | Id Identifier deriving (Show, Eq)

-- test :: TokenType -> String
-- test (Lit x) = show x
-- test (Kw x) = show x
-- test (Sym x) = show x
