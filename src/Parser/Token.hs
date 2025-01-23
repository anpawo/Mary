{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Token
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE RecordWildCards #-}

module Parser.Token (MyToken (..), Type (..), Literal(..), Identifier(..), SubExpression(..)) where

import Text.Printf (printf)
import Data.List (intercalate)

data SubExpression
  = VariableCall { varCallName :: String }
  | FunctionCall { fnCallName :: String, fnCallArgs :: [SubExpression]}
  | Lit Literal
  deriving (Show, Eq, Ord)

data Type
  = CharType --                                                        \| char
  | NullType --                                                        \| null
  | VoidType --                                                        \| void
  | BoolType --                                                        \| bool
  | IntType --                                                         \| int
  | FloatType --                                                       \| float
  | StrType --                                                         \| str -- todo: arr [ char ]
  | ArrType Type --                                                    \| arr [ <type> ]
  | AnyType --                                                         \| any
  | StructType { stTyName :: String} --                                \| struct <name> (the real parsing of the structure is done in the Ast)
  | ConstraintType { crTyName :: Maybe String, crTyTypes :: [Type]} -- \| int | float
  | FunctionType { fnTyArgs :: [Type], fnTyRet :: Type }
  deriving (Ord)

instance Eq Type where
  (StructType n) == (StructType n') = n == n'
  (ArrType t) == (ArrType t') = t == t'
  (FunctionType argsTy retTy) == (FunctionType argsTy' retTy') = argsTy == argsTy' && retTy == retTy'
  AnyType == (FunctionType {}) = False
  AnyType == _ = True
  (FunctionType {}) == AnyType = False
  _ == AnyType = True
  (ConstraintType (Just n) _) == (ConstraintType (Just n') _) | n == n' = True
  c@(ConstraintType _ _) == (ConstraintType _ ts) = c `elem` ts
  (ConstraintType _ ts) == t = t `elem` ts
  t == (ConstraintType _ ts) = t `elem` ts
  CharType == CharType = True
  NullType == NullType = True
  VoidType == VoidType = True
  BoolType == BoolType = True
  IntType == IntType = True
  FloatType == FloatType = True
  StrType == StrType = True
  _ == _ = False

instance Show Type where
  show (FunctionType argsTy retTy) = printf "(%s) -> %s" (intercalate ", " $ map show argsTy) (show retTy)
  show CharType = "char"
  show NullType = "null"
  show VoidType = "void"
  show BoolType = "bool"
  show IntType = "int"
  show FloatType = "float"
  show StrType = "str"
  show (ArrType t) = printf "arr[%s]" $ show t
  show (StructType n) = printf "%s" n
  show AnyType = "any"
  show (ConstraintType (Just n) _) = n
  show (ConstraintType Nothing t) = intercalate " | " (map show t)

data Literal
  = CharLit Char --                               \| 'c'   -> may be a list of char
  | BoolLit Bool --                               \| true | false
  | IntLit { intLiteralValue :: Int} --           \| 2
  | FloatLit Double --                            \| 1.5
  | StringLit String --                           \| "yo"   -> may be a list of char
  | ArrLitPre Type [[MyToken]] --                 \| before computation of the elements
  | ListLitPre [[MyToken]] --                     \| before computation of the elements
  | ArrLit Type [SubExpression] --                \| [1, 2, 3]
  | StructLitPre String [(String, [MyToken])] --  \| before computation of the elements
  | StructLit String [(String, SubExpression)] -- \| {name: "marius", age: 19}
  | NullLit --                                    \| null
  | ClosureLit String [Type] Type --              \| (+)
  | LambdaLit { lambdaCapture :: [String], lambdaArgs :: [(Type, String)], lambdaBody :: SubExpression, lambdaRetTy :: Type} -- \| (\(a: int, b: int) -> int = a + b)
  deriving (Eq, Ord)

instance Show Literal where
  show (ClosureLit x _ _) = printf "(%s)" x
  show (CharLit x) = show x
  show (BoolLit True) = "true"
  show (BoolLit False) = "false"
  show (IntLit x) = show x
  show (FloatLit x) = show x
  show (StringLit x) = show x
  show (ArrLitPre n x) = printf "%s%s" (show n) (show $ map head x)
  show (ArrLit t x) = printf "%s [%s]" (show t) $ intercalate ", " $ map show x
  show (StructLitPre n x) = printf "%s { %s }" n $ intercalate ", " $ map (\(n', v) -> printf "%s = %s" n' (unwords $ map show v)) x
  show (StructLit n x) = printf "%s { %s }" n $ intercalate ", " $ map (\(k, v) -> printf "%s = %s" k (show v)) x
  show NullLit = "NULL"
  show (ListLitPre a) = printf "%s" $ show $ map head a
  show (LambdaLit {..}) = printf "%s(%s) -> %s" (show lambdaCapture) (show lambdaArgs) (show lambdaRetTy)

data Identifier
  = TextId { textIdName :: String} --   \| factorial, add_2, x
  | OperatorId { opIdName :: String } -- \| <*>, +, *, **
  deriving (Eq, Ord)

instance Show Identifier where
  show (TextId x) = x
  show (OperatorId x) = x

data MyToken
  =
  -- Keyword
    FunctionKw --        \| function     -> declare a function
  | OperatorKw --        \| operator     -> declare an operator
  | TypeKw --            \| type         -> declare a type constraint
  | PrecedenceKw --      \| precedence   -> declare an operator precedence
  | ImportKw String --   \| import <x>   -> for imports
  | BuiltinKw --         \| builtin      -> for builtins
  | IfKw --              \| if           -> if
  | ThenKw --            \| then         -> then
  | ElseKw --            \| else         -> else
  | WhileKw --           \| while        -> while
  | ReturnKw --          \| return       -> return
  | AtomKw --            \| atom         -> empty structure
  -- Symbol
  | CurlyOpen --         \|  {   -> struct definition and creation start
  | CurlyClose --        \|  }   -> struct definition and creation end
  | ParenOpen --         \|  (   -> group start
  | ParenClose --        \|  )   -> group end
  | BracketOpen --       \|  [   -> array start
  | BracketClose --      \|  ]   -> array end
  | Arrow --             \|  ->  -> return type of functions
  | SemiColon --         \|  ;   -> end of statement
  | Colon --             \|  :   -> definition in structure
  | Comma --             \|  ,   -> separate arguments for functions
  | Pipe --              \|  |   -> separate types for constraints
  | Assign --            \|  =   -> compound assignment operators
  | AssignAdd --         \|  +=  -> compound assignment operators
  | AssignSub --         \|  -=  -> compound assignment operators
  | AssignMul --         \|  *=  -> compound assignment operators
  | AssignDiv --         \|  /=  -> compound assignment operators
  | BackSlash --         \|  \   -> lambda creation
  -- Type
  | Type { typing :: Type }
  -- Literal
  | Literal { literal :: Literal }
  -- Identifier
  | Identifier { identifier :: Identifier }
  deriving (Eq, Ord)

instance Show MyToken where
  show FunctionKw = "function"
  show OperatorKw = "operator"
  show TypeKw = "type"
  show PrecedenceKw = "precedence"
  show (ImportKw name) = "import " ++ name
  show BuiltinKw = "builtin"
  show IfKw = "if"
  show ThenKw = "then"
  show ElseKw = "else"
  show WhileKw = "while"
  show ReturnKw = "return"
  show AtomKw = "atom"

  show CurlyOpen = "{"
  show CurlyClose = "}"
  show ParenOpen = "("
  show ParenClose = ")"
  show BracketOpen = "["
  show BracketClose = "]"
  show Arrow = "->"
  show SemiColon = ";"
  show Colon = ":"
  show Comma = ","
  show Pipe = "|"
  show Assign = "="
  show AssignAdd = "+="
  show AssignSub = "-="
  show AssignMul = "*="
  show AssignDiv = "/="
  show BackSlash = "\\"

  show (Type t) = show t
  show (Literal l) = show l
  show (Identifier i) = show i
