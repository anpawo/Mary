{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Parser.Tokenizer
    (
    -- main
    Parser,
    comment,
    macro,
    tokenize,

    -- test
    dbg,
    parseTest,

    ) where

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>), (<&>))

import Control.Applicative ((<|>), some, empty, optional)
import Control.Monad (void, liftM2)

import Text.Megaparsec (Parsec, many, manyTill, anySingle, eof, parseTest, manyTill_, (<?>), oneOf, notFollowedBy, try, someTill, MonadParsec (lookAhead, hidden), SourcePos (..), getSourcePos)
import Text.Megaparsec.Char (char, string, alphaNumChar, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal, float)
import Text.Megaparsec.Debug (dbg)

import Parser.Token
import Utils.Lib (choicetry, (~>), (&>))
import Text.Megaparsec.Pos (unPos)
import Ast.Error (red)

type Parser = Parsec Void String

-- utils
skipString :: Parser String
skipString = (\s -> '"' : s ++ "\"" ) <$> (quote *> manyTill anySingle (quote <?> "closing quote `\"` of the string."))

quote :: Parser Char
quote = char '"'

singlequote :: Parser Char
singlequote = char '\''

keyword :: String -> Parser String
keyword s = string s <* notFollowedBy textIdentifier

symbol :: String -> Parser String
symbol s = string s <* notFollowedBy operatorIdentifier

textIdentifier :: Parser Char
textIdentifier = alphaNumChar <|> underscore

operatorIdentifier :: Parser Char
operatorIdentifier = oneOf ['+', '-', '*', '/', '<', '>', '|', '^', '&', '~', '!', '$' , '.', '=', ':', '%']

underscore :: Parser Char
underscore = char '_'

spaces :: Parser String
spaces = many $ oneOf " \n\t"

linespaces :: Parser String
linespaces = some $ oneOf " \t"

toSpace :: Char -> Char
toSpace '\n' = '\n'
toSpace _ = ' '
-- utils

-- comments
comment :: Parser String
comment = concat <$> manyTill (skipString <|> lineComment <|> blockComment <|> (singleton <$> anySingle)) eof
    where
        lineComment :: Parser String
        lineComment =  startComment *> manyTill_ anySingle endComment >>= \(s, e) -> pure $ "  " ++ map toSpace s ++ e
            where
                startComment = string "//"
                endComment = string "\n" <|> eof $> ""

        blockComment :: Parser String
        blockComment = startComment *> manyTill anySingle endComment >>= \s -> pure $ "  " ++ map toSpace s ++ "  "
            where
                startComment :: Parser String
                startComment = string "/*"

                endComment :: Parser String
                endComment = string "*/" <?> "end of multi-line comment \"*/\"."
-- comments

-- macro (may need to add space instead of removing everything because the parser will indicate the wrong place otherwise)
type Before = String
type After = String

macro ::  Parser String
macro = getMacros ~> applyMacros
    where
        getMacros :: Parser ([(Before, After)], String)
        getMacros = temp [] ""
            where
                temp :: [(Before, After)] -> String -> Parser ([(Before, After)], String)
                temp macros str =
                        (eof >> return (macros, str))
                    <|> (skipString >>= \s -> temp macros (str ++ s))
                    <|> (simpleMacro >>= \m -> temp (m : macros) str)
                    <|> (anySingle >>= \c -> temp macros (str ++ [c]))

        simpleMacro :: Parser (Before, After)
        simpleMacro = (,) <$> (string macroPrefix *> linespaces *> someTill anySingle linespaces) <*> manyTill anySingle (void (char '\n') <|> eof)

        macroPrefix = "macro"

        applyMacros :: [(Before, After)] -> Parser String
        applyMacros macros = concat <$> manyTill (applyOne macros <|> (singleton <$> anySingle)) eof
            where
                applyOne :: [(Before, After)] -> Parser String
                applyOne [] = empty
                applyOne ((a, b): rst) = string a $> b <|> applyOne rst
-- macro

-- -- namespace (working but may be useless according to what we decide to do)
-- type Import = String

-- namespace :: Parser String
-- namespace = getImport ~> applyNamespace
--     where
--         getImport :: Parser ([Import], String)
--         getImport = temp [] "" where
--             temp :: [Import] -> String -> Parser ([Import], String)
--             temp imports str =
--                     (eof >> return (imports, str))
--                 <|> (skipString >>= \s -> temp imports (str ++ s))
--                 <|> (singleImport >>= \m -> temp (m : imports) str)
--                 <|> (anySingle >>= \c -> temp imports (str ++ [c]))

--         singleImport :: Parser Import
--         singleImport = string importPrefix *> linespaces *> manyTill anySingle (void (char '\n') <|> eof)

--         importPrefix = "import"

--         applyNamespace :: [Import] -> Parser String
--         applyNamespace imports = concat <$> manyTill (applyOne imports <|> (singleton <$> anySingle)) eof
--             where
--                 applyOne :: [Import] -> Parser String
--                 applyOne [] = empty
--                 applyOne (name: rst) = (string (name ++ ".") $> ("_ZN" ++ show (length name) ++ name)) <|> applyOne rst
-- -- namespace

type Line = Int
type Col  = Int

-- TokenType
tokenize :: Parser ([(Line, Col)], [MyToken])
tokenize = comment &> macro &> (unzip <$> (spaces *> manyTill (((,) <$> pos <*> (tokens <?> red "a known symbol")) <* spaces) (hidden eof)))
    where
        pos = (\p -> (unPos $ sourceLine p, unPos $ sourceColumn p)) <$> getSourcePos

        tokens = choicetry
            [
            -- Symbol
               curlyOpenSym
            ,  curlyCloseSym
            ,  parenOpenSym
            ,  parenCloseSym
            ,  arrowSym
            ,  semicolonSym
            ,  colonSym
            ,  commaSym
            ,  pipeSym
            ,  assignSym
            ,  assignAddSym
            ,  assignSubSym
            ,  assignMulSym
            ,  assignDivSym
            ,  backslashSym

            -- Keyword
            , functionKw
            , operatorKw
            , precedenceKw
            , importKw
            , builtinKw
            , ifKw
            , thenKw
            , elseKw
            , whileKw
            , returnKw
            , atomKw

            -- Literal
            , Literal <$> parseLit

            -- Temp
            ,  bracketOpenSym
            ,  bracketCloseSym

            -- Type
            , Type <$> parseType

            -- Identifier
            , symbolId
            , operatorId

            ]

        -- maybe types should be parsed before literals

        -- Literal
        parseLit = choicetry [
              CharLit <$> (singlequote *> asciiChar <* singlequote)
            , BoolLit <$> ((string "true" $> True) <|> (string "false" $> False))
            , FloatLit <$> (((0 -) <$> (char '-' *> float)) <|> float)
            , IntLit <$> (((0 -) <$> (char '-' *> decimal)) <|> decimal)
            , StringLit <$> (quote *> manyTill anySingle quote)
            , NullLit <$ string "NULL"
            , do
                ty <- optional parseType
                args <- spaces *> char '[' *> spaces *> ((char ']' $> []) <|> arrayArgs)
                case ty of
                    Nothing -> return $ ListLitPre args
                    Just ty' -> return $ ArrLitPre ty' args
            , do
                name <- some textIdentifier <* spaces
                args <- char '{' *> spaces *> ((char '}' $> []) <|> structArgs)
                return $ StructLitPre name args
            ]
            where
                structArgs :: Parser [(String, [MyToken])]
                structArgs = (:) <$> liftM2 (,) (spaces *> some textIdentifier <* spaces <* char '=') (oneArg 0 0 0) <*> ((char ',' *> structArgs) <|> (char '}' $> []))
                    where
                        -- in function / array / struct
                        oneArg :: Int -> Int -> Int -> Parser [MyToken]
                        oneArg inFunction inArray inStruct = do
                            let oneTok = spaces *> tokens
                            t <- lookAhead oneTok
                            case t of
                                ParenOpen    -> (:) <$> oneTok <*> oneArg (inFunction + 1) inArray inStruct
                                ParenClose   -> (:) <$> oneTok <*> oneArg (inFunction - 1) inArray inStruct
                                BracketOpen  -> (:) <$> oneTok <*> oneArg inFunction (inArray + 1) inStruct
                                BracketClose -> (:) <$> oneTok <*> oneArg inFunction (inArray - 1) inStruct
                                CurlyOpen    -> (:) <$> oneTok <*> oneArg inFunction inArray (inStruct + 1)
                                CurlyClose   -> if inFunction == 0 && inArray == 0 && inStruct == 0
                                    then spaces $> []
                                    else (:) <$> oneTok <*> oneArg inFunction inArray (inStruct - 1)
                                Comma        -> if inFunction == 0 && inArray == 0 && inStruct == 0
                                    then spaces $> []
                                    else (:) <$> oneTok <*> oneArg inFunction inArray inStruct
                                _ -> (:) <$> oneTok <*> oneArg inFunction inArray inStruct

                arrayArgs :: Parser [[MyToken]]
                arrayArgs = (:) <$> oneArg 0 0 0 <*> ((char ',' *> arrayArgs) <|> (char ']' $> []))
                    where
                        -- in function / array / struct
                        oneArg :: Int -> Int -> Int -> Parser [MyToken]
                        oneArg inFunction inArray inStruct = do
                            let oneTok = spaces *> tokens <* spaces
                            t <- lookAhead oneTok
                            case t of
                                ParenOpen    -> (:) <$> oneTok <*> oneArg (inFunction + 1) inArray inStruct
                                ParenClose   -> (:) <$> oneTok <*> oneArg (inFunction - 1) inArray inStruct
                                CurlyOpen    -> (:) <$> oneTok <*> oneArg inFunction inArray (inStruct + 1)
                                CurlyClose   -> (:) <$> oneTok <*> oneArg inFunction inArray (inStruct - 1)
                                BracketOpen  -> (:) <$> oneTok <*> oneArg inFunction (inArray + 1) inStruct
                                BracketClose   -> if inFunction == 0 && inArray == 0 && inStruct == 0
                                    then spaces $> []
                                    else (:) <$> oneTok <*> oneArg inFunction (inArray - 1) inStruct
                                Comma        -> if inFunction == 0 && inArray == 0 && inStruct == 0
                                    then spaces $> []
                                    else (:) <$> oneTok <*> oneArg inFunction inArray inStruct
                                _ -> (:) <$> oneTok <*> oneArg inFunction inArray inStruct

        -- Symbol
        curlyOpenSym = char '{' $> CurlyOpen
        curlyCloseSym = char '}' $> CurlyClose
        parenOpenSym = char '(' $> ParenOpen
        parenCloseSym = char ')' $> ParenClose
        bracketOpenSym = char '[' $> BracketOpen
        bracketCloseSym = char ']' $> BracketClose
        semicolonSym = char ';' $> SemiColon
        colonSym = try $ symbol ":" $> Colon
        commaSym = char ',' $> Comma
        arrowSym = try $ symbol "->" $> Arrow
        pipeSym = try $ symbol "|" $> Pipe
        assignSym = try $ symbol "=" $> Assign
        assignAddSym = try $ symbol "+=" $> AssignAdd
        assignSubSym = try $ symbol "-=" $> AssignSub
        assignMulSym = try $ symbol "*=" $> AssignMul
        assignDivSym = try $ symbol "/=" $> AssignDiv
        backslashSym = char '\\' $> BackSlash

        -- Keyword
        functionKw = try $ keyword "function" $> FunctionKw
        operatorKw = try $ keyword "operator" $> OperatorKw
        precedenceKw =  try $ keyword "precedence" $> PrecedenceKw
        importKw =  try $ ImportKw <$> (keyword "import" *> spaces *> some textIdentifier)
        builtinKw =  try $ keyword "builtin" $> BuiltinKw
        ifKw = try $ keyword "if" $> IfKw
        thenKw = try $ keyword "then" $> ThenKw
        elseKw = try $ keyword "else" $> ElseKw
        whileKw = try $ keyword "while" $> WhileKw
        returnKw = try $ keyword "return" $> ReturnKw
        atomKw = try $ keyword "atom" $> AtomKw

        -- Type
        parseType = choicetry [
              keyword "any" $> AnyType
            , keyword "null" $> NullType
            , keyword "char" $> CharType
            , keyword "void" $> VoidType
            , keyword "bool" $> BoolType
            , keyword "int" $> IntType
            , keyword "float" $> FloatType
            , keyword "str" $> StrType
            , keyword "arr" *> spaces *> char '[' *> spaces *> parseType <* spaces <* char ']' <&> ArrType
            , keyword "struct" *> spaces *> some textIdentifier <&> StructType
            , keyword "type" *> spaces *> some textIdentifier <&> (`ConstraintType` []) . Just
            ]

        -- Identifier
        symbolId = Identifier . TextId <$> some textIdentifier -- names cannot start as number because they will just be parsed as int first
        operatorId = Identifier . OperatorId <$> some operatorIdentifier
-- TokenType
