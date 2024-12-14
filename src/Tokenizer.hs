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
    t,
    runParser,

    ) where

-- import Debug.Trace (trace)

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Text.Megaparsec (Parsec, many, manyTill, anySingle, eof, parseTest, manyTill_, runParser, (<?>), oneOf, setInput, choice, notFollowedBy, try)
import Text.Megaparsec.Char (char, string, alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal, float)

import Token

type Parser = Parsec Void String

-- Parsec err input = ParsecT err input Identity
-- ParsecT supports I/O
-- Parsec has Identity for monad so no I/O

-- used for tests
t :: Show a => Parser a -> String -> IO ()
t = parseTest
-- used for tests


-- utils
skipString :: Parser String
skipString = (\s -> '"' : s ++ "\"" ) <$> (quote *> manyTill (escaped <|> anySingle) (quote <?> "closing quote `\"` of the string."))

between :: Parser a -> Parser b -> Parser a
between  p around = around *> p <* around

escaped :: Parser Char
escaped = escape *> choice [quote, escape]

quote :: Parser Char
quote = char '"'

keyword :: String -> Parser String
keyword s = string s <* notFollowedBy prefixIdentifierChar

prefixIdentifierChar :: Parser Char
prefixIdentifierChar = alphaNumChar <|> underscore

infixIdentifierChar :: Parser Char
infixIdentifierChar = oneOf ['+', '-', '*', '/', '<', '>', '|', '^', '&', '~', '!', '$', '.']

underscore :: Parser Char
underscore = char '_'

escape :: Parser Char
escape = char '\\'

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
tokenize = manyTill (spaces *> tokens) eof
    where
        tokens = choice
            [
              -- Literal
              intLit
            , fltLit
            , strLit

            -- Symbol
            ,  curtlyOpenSym
            ,  curtlyCloseSym
            ,  parenOpenSym
            ,  parenCloseSym
            ,  assignSym
            ,  semicolonSym
            ,  scopeSym -- not sure if needed

            -- Keyword
            , functionKw
            , infixKw
            , structKw
            , importKw
            , asKw

            -- Identifier
            , prefixId
            , infixId
            ]

        -- Literal
        intLit = IntLit <$> (try ((0 -) <$> (char '-' *> decimal)) <|> try decimal)
        fltLit = FloatLit <$> (try ((0 -) <$> (char '-' *> float)) <|> try float)
        strLit = StringLit <$> manyTill (escaped <|> anySingle) quote `between` (quote <?> "closing quote `\"` of the string.")

        -- Symbol
        curtlyOpenSym =  char '{' $> CurlyOpen
        curtlyCloseSym =  char '}' $> CurlyClose

        parenOpenSym =  char '(' $> CurlyOpen
        parenCloseSym =  char ')' $> CurlyClose

        assignSym =  char '=' $> CurlyClose
        scopeSym =  char '.' $> CurlyClose
        semicolonSym = char ';' $> CurlyClose

        -- Keyword
        functionKw =  keyword "fn" $> FnKw
        infixKw =  keyword "fn" $> FnKw
        structKw = keyword "struct" $> StructKw
        importKw =  keyword "import" $> ImportKw
        asKw = keyword "as" $> AsKw

        -- Identifier
        prefixId = PrefixId <$> some prefixIdentifierChar
        infixId = InfixId <$> some infixIdentifierChar
-- TokenType


-- TODO
-- complexMacro
