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
    (&>),

    -- test
    t,
    Parser,
    runParser,
    parseTest,
    setInput,
    eof,

    ) where

-- import Debug.Trace (trace)

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Text.Megaparsec (Parsec, many, noneOf, manyTill, anySingle, eof, parseTest, manyTill_, runParser, (<?>), oneOf, ParsecT, setInput)
import Text.Megaparsec.Char (char, string)
-- import Text.Megaparsec.Char.Lexer (decimal, float, signed)

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
skipString = (\s -> '"' : s ++ "\"" ) <$> (char '"' *> many (noneOf "\"") <* (char '"' <?> errmsg))
    where
        errmsg = "closing quote `\"` of the string."

-- space :: Parser Char
-- space = oneOf " \n\t"

linespaces :: Parser String
linespaces = some $ oneOf " \t"

(~>) :: Parser (a, String) -> (a -> Parser b) -> Parser b
(~>) p1 p2 = p1 >>= (\(out, input) -> setInput input >> p2 out)

(&>) :: Parser String -> Parser String -> Parser String
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

-- macro
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



-- TODO
-- complexMacro
