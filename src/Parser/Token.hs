{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- MyToken
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser.Token (MyToken (..), Type (..), Literal(..), Identifier(..), SubExpression(..)) where

import Text.Printf (printf)
import Data.List (intercalate)

data SubExpression
  = VariableCall { varName :: String }
  | FunctionCall { fnName :: String, fnArgs :: [SubExpression]}
  | Lit Literal
  deriving (Show, Eq, Ord)

data Type
  = CharType --                     \| char
  | VoidType --                     \| void
  | BoolType --                     \| bool
  | IntType --                      \| int
  | FloatType --                    \| float
  | StrType --                      \| str -- todo: arr [ char ]
  | ArrType Type --                 \| arr [ <type> ]
  | StructType String --            \| struct <name> (the real parsing of the structure is done in the Ast)
  | AnyType --                      \| any (only for builtins)
  | StructAnyType --                \| struct any (only for builtins)
  | ConstraintType String [Type] -- \| int | float (only for builtins)
  deriving (Ord)

instance Eq Type where
  StructAnyType == (StructType _) = True
  (StructType _) == StructAnyType = True
  (StructType n) == (StructType n') = n == n'
  (ArrType t) == (ArrType t') = t == t'
  AnyType == _ = True
  _ == AnyType = True
  (ConstraintType {}) == (ConstraintType {}) = False -- todo
  CharType == CharType = True
  VoidType == VoidType = True
  BoolType == BoolType = True
  IntType == IntType = True
  FloatType == FloatType = True
  StrType == StrType = True
  _ == _ = False

instance Show Type where
  show CharType = "char"
  show VoidType = "void"
  show BoolType = "bool"
  show IntType = "int"
  show FloatType = "float"
  show StrType = "str"
  show (ArrType t) = printf "arr[%s]" $ show t
  show (StructType n) = printf "struct %s" n
  show AnyType = "any"
  show StructAnyType = "struct any"
  show (ConstraintType n t) = printf "%s = %s" n $ intercalate " | " $ map show t

data Literal
  = CharLit Char --                                                 \| 'c'   -> may be a list of char
  | BoolLit Bool --                                                 \| true | false
  | IntLit Int --                                                   \| 2
  | FloatLit Double --                                              \| 1.5
  | StringLit String --                                             \| "yo"   -> may be a list of char
  | ArrLitPre Type [[MyToken]] --                                   \| before computation of the elements
  | ArrLit Type [SubExpression] --                                  \| [1, 2, 3]
  | StructLitPre String [(String, [MyToken])] --                    \| before computation of the elements
  | StructLit String [(String, SubExpression)] --                       \| {name: "marius", age: 19}
  deriving (Eq, Ord)

instance Show Literal where
  show (CharLit x) = show x
  show (BoolLit True) = "true"
  show (BoolLit False) = "false"
  show (IntLit x) = show x
  show (FloatLit x) = show x
  show (StringLit x) = show x
  show (ArrLitPre n x) = printf "%s [%s]" (show n) (show x)
  show (ArrLit t x) = printf "%s [%s]" (show t) $ intercalate ", " $ map show x
  show (StructLitPre n x) = printf "%s { %s }" n $ intercalate ", " $ map (\(n', v) -> printf "%s = %s" n' (unwords $ map show v)) x
  show (StructLit n x) = printf "%s { %s }" n $ intercalate ", " $ map (\(k, v) -> printf "%s = %s" k (show v)) x

data Identifier
  = SymbolId String --   \| factorial, add_2, x
  | OperatorId String -- \| <*>, +, *, **
  deriving (Eq, Ord)

instance Show Identifier where
  show (SymbolId x) = x
  show (OperatorId x) = x

data MyToken
  =
  -- Keyword
    FunctionKw --        \| function     -> declare a function
  | OperatorKw --        \| operator     -> declare an operator
  | ConstraintKw --      \| constraint   -> declare a constraint
  | PrecedenceKw --      \| precedence   -> declare an operator precedence
  | ImportKw --          \| import       -> for imports (bonus)
  | IfKw --              \| if           -> if
  | ThenKw --            \| then         -> then
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
  | SemiColon --         \|  ;   -> end of statement
  | Comma --             \|  ,   -> separate arguments in function call/creation
  -- Type
  | Type Type
  -- Literal
  | Literal Literal
  -- Identifier
  | Identifier Identifier
  deriving (Eq, Ord)

instance Show MyToken where
  show FunctionKw = "function"
  show OperatorKw = "operator"
  show ConstraintKw = "constraint"
  show PrecedenceKw = "precedence"
  show ImportKw = "import"
  show IfKw = "if"
  show ThenKw = "then"
  show ElseKw = "else"
  show ReturnKw = "return"

  show CurlyOpen = "{"
  show CurlyClose = "}"
  show ParenOpen = "("
  show ParenClose = ")"
  show BracketOpen = "["
  show BracketClose = "]"
  show Assign = "="
  show Arrow = "->"
  show SemiColon = ";"
  show Comma = ","

  show (Type t) = show t
  show (Literal l) = show l
  show (Identifier i) = show i
