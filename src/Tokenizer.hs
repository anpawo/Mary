{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Tokenizer
    (
    -- main
    comment,
    macro,
    namespace,
    tokenize,
    (&>),

    -- test
    dbg,
    runParser,
    parseTest,

    ) where

-- import Debug.Trace (trace)

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Text.Megaparsec (Parsec, many, manyTill, anySingle, eof, parseTest, manyTill_, runParser, (<?>), oneOf, setInput, choice, notFollowedBy, try)
import Text.Megaparsec.Char (char, string, alphaNumChar, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal, float)
import Text.Megaparsec.Debug (dbg)

import Token

type Parser = Parsec Void String

-- Parsec err input = ParsecT err input Identity
-- ParsecT supports I/O
-- Parsec has Identity for monad so no I/O


-- utils
skipString :: Parser String
skipString = (\s -> '"' : s ++ "\"" ) <$> (quote *> manyTill anySingle (quote <?> "closing quote `\"` of the string."))

-- escaped :: Parser Char
-- escaped = escape *> choice [quote, escape]

-- escape :: Parser Char
-- escape = char '\\'

quote :: Parser Char
quote = char '"'

singlequote :: Parser Char
singlequote = char '\''

keyword :: String -> Parser String
keyword s = string s <* notFollowedBy prefixIdentifierChar

symbol :: String -> Parser String
symbol s = string s <* notFollowedBy infixIdentifierChar

prefixIdentifierChar :: Parser Char
prefixIdentifierChar = alphaNumChar <|> underscore

infixIdentifierChar :: Parser Char
infixIdentifierChar = oneOf ['+', '-', '*', '/', '<', '>', '|', '^', '&', '~', '!', '$', '.']

underscore :: Parser Char
underscore = char '_'

spaces :: Parser String
spaces = many $ oneOf " \n\t"

linespaces :: Parser String
linespaces = some $ oneOf " \t"

-- combine p1 and p2 while updating the input, p2 needs a from (p1 => (a, b))
(~>) :: Parser (a, String) -> (a -> Parser b) -> Parser b
(~>) p1 p2 = p1 >>= (\(out, input) -> setInput input >> p2 out)

-- combine p1 and p2 while updating the input
(&>) :: Parser String -> Parser a -> Parser a
(&>) p1 p2 = p1 >>= (\s -> setInput s >> p2)
-- utils

-- comments
comment :: Parser String
comment = concat <$> manyTill (skipString <|> lineComment <|> blockComment <|> (singleton <$> anySingle)) eof
    where
        lineComment :: Parser String
        lineComment =  snd <$> (startComment *> manyTill_ anySingle endComment)
            where
                startComment = string "//"
                endComment = string "\n" <|> eof $> ""

        blockComment :: Parser String
        blockComment = startComment *> manyTill anySingle endComment $> ""
            where
                startComment :: Parser String
                startComment = string "/*"

                endComment :: Parser String
                endComment = string "*/" <?> errmsg

                errmsg = "end of multi-line comment \"*/\"."
-- comments

-- macro (may need to add space instead of removing everything because the parser will indicate the wrong place otherwise)
type Before = String
type After = String

macro ::  Parser String
macro = getSimple ~> applySimple
    where
        getSimple :: Parser ([(Before, After)], String)
        getSimple = temp [] "" where
            temp :: [(Before, After)] -> String -> Parser ([(Before, After)], String)
            temp macros str =
                    (eof >> return (macros, str))
                <|> (skipString >>= \s -> temp macros (str ++ s))
                <|> (simpleMacro >>= \m -> temp (m : macros) str)
                <|> (anySingle >>= \c -> temp macros (str ++ [c]))

        simpleMacro :: Parser (Before, After)
        simpleMacro = (,) <$> (string macroPrefix *> linespaces *> manyTill anySingle linespaces) <*> manyTill anySingle (void (char '\n') <|> eof)

        macroPrefix = "macro"

        applySimple :: [(Before, After)] -> Parser String
        applySimple macros = concat <$> manyTill (applyOne macros <|> (singleton <$> anySingle)) eof
            where
                applyOne :: [(Before, After)] -> Parser String
                applyOne [] = empty
                applyOne ((a, b): rst) = string a $> b <|> applyOne rst
-- macro

-- namespace (working but may be useless according to what we decide to do)
type Import = String

namespace :: Parser String
namespace = getImport ~> applyNamespace
    where
        getImport :: Parser ([Import], String)
        getImport = temp [] "" where
            temp :: [Import] -> String -> Parser ([Import], String)
            temp imports str =
                    (eof >> return (imports, str))
                <|> (skipString >>= \s -> temp imports (str ++ s))
                <|> (singleImport >>= \m -> temp (m : imports) str)
                <|> (anySingle >>= \c -> temp imports (str ++ [c]))

        singleImport :: Parser Import
        singleImport = string importPrefix *> linespaces *> manyTill anySingle (void (char '\n') <|> eof)

        importPrefix = "import"

        applyNamespace :: [Import] -> Parser String
        applyNamespace imports = concat <$> manyTill (applyOne imports <|> (singleton <$> anySingle)) eof
            where
                applyOne :: [Import] -> Parser String
                applyOne [] = empty
                applyOne (name: rst) = (string (name ++ ".") $> ("_ZN" ++ show (length name) ++ name)) <|> applyOne rst
-- namespace


-- TokenType
tokenize :: Parser [Token]
tokenize = spaces *> manyTill (tokens <* spaces) eof
    where
        tokens = choice
            [
              -- Literal
              charLit
            , boolLit
            , intLit
            , fltLit
            , strLit

            -- Symbol
            ,  curlyOpenSym
            ,  curlyCloseSym
            ,  parenOpenSym
            ,  parenCloseSym
            ,  assignSym
            ,  arrowSym
            ,  semicolonSym
            ,  scopeSym -- not sure if needed

            -- Keyword
            , functionKw
            , infixKw
            , structKw
            , importKw
            , asKw
            , isKw
            , atKw

            -- Identifier
            , symbolId
            , operatorId

            -- Type
            , charT
            , boolT
            , intT
            , floatT
            , strT
            , arrT

            ]

        -- Literal
        charLit = CharLit <$> try (singlequote *> asciiChar <* (singlequote <?> "closing singlequote `\'` of the char."))
        boolLit = BoolLit <$> (try (string "true" $> True) <|> try (string "false" $> False))
        intLit = IntLit <$> (try ((0 -) <$> (char '-' *> decimal)) <|> try decimal)
        fltLit = FloatLit <$> (try ((0 -) <$> (char '-' *> float)) <|> try float)
        strLit = StringLit <$> try (quote *> manyTill anySingle (quote <?> "closing quote `\"` of the string."))

        -- Symbol
        curlyOpenSym =  char '{' $> CurlyOpen
        curlyCloseSym =  char '}' $> CurlyClose

        parenOpenSym =  char '(' $> ParenOpen
        parenCloseSym =  char ')' $> ParenClose

        assignSym =  symbol "=" $> Assign
        arrowSym =  symbol "->" $> Assign
        scopeSym =  symbol "." $> Scope
        semicolonSym = char ';' $> SemiColon

        -- Keyword
        functionKw =  try $ keyword "fn" $> FunctionKw
        infixKw =  try $ keyword "fn" $> FunctionKw
        structKw = try $ keyword "struct" $> StructKw
        importKw =  try $ keyword "import" $> ImportKw
        asKw = try $ keyword "as" $> AsKw
        isKw = try $ keyword "is" $> IsKw
        atKw = try $ keyword "at" $> AtKw

        -- Type
        charT = try $ keyword "char" $> CharT
        boolT = try $ keyword "bool" $> BoolT
        intT = try $ keyword "int" $> IntT
        floatT = try $ keyword "float" $> FloatT
        strT = try $ keyword "str" $> StrT
        arrT = try $ keyword "arr" $> ArrT

        -- Identifier
        symbolId = try $ SymbolId <$> some prefixIdentifierChar
        operatorId = try $ OperatorId <$> some infixIdentifierChar
-- TokenType


-- TODO
-- complexMacro
