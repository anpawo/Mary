{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Parser.Tokenizer
    (
    -- main
    run,
    Parser,
    comment,
    macro,
    namespace,
    tokenize,
    (&>),

    -- test
    dbg,
    parseTest,

    ) where

-- import Debug.Trace (trace)

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>), some, empty)
import Control.Monad (void)

import Text.Megaparsec (Parsec, ParseErrorBundle, many, manyTill, anySingle, eof, parseTest, manyTill_, runParser, (<?>), oneOf, setInput, choice, notFollowedBy, try, someTill)
import Text.Megaparsec.Char (char, string, alphaNumChar, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal, float)
import Text.Megaparsec.Debug (dbg)

import Parser.Token

type Parser = Parsec Void String

-- Parsec err input = ParsecT err input Identity
-- ParsecT supports I/O
-- Parsec has Identity for monad so no I/O


-- utils
run :: Parser a -> String -> Either (ParseErrorBundle String Void) a
run parser = runParser parser ""

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
              Literal <$> charLit
            , Literal <$> boolLit
            , Literal <$> fltLit
            , Literal <$> intLit
            , Literal <$> strLit

            -- Symbol
            ,  curlyOpenSym
            ,  curlyCloseSym
            ,  parenOpenSym
            ,  parenCloseSym
            ,  bracketOpenSym
            ,  bracketCloseSym
            ,  assignSym
            ,  arrowSym
            ,  semicolonSym
            ,  commaSym
            ,  scopeSym -- not sure if needed

            -- Keyword
            , functionKw
            , operatorKw
            , precedenceKw
            , structKw
            , isKw
            , importKw
            , asKw
            , atKw
            , ifKw
            , elseKw
            , returnKw

            -- Type
            , Type <$> charT
            , Type <$> voidT
            , Type <$> boolT
            , Type <$> intT
            , Type <$> floatT
            , Type <$> strT
            , Type <$> arrT

            -- Identifier
            , Identifier <$> symbolId
            , Identifier <$> operatorId

            ]

        -- Literal
        charLit = CharLit <$> try (singlequote *> asciiChar <* (singlequote <?> "closing singlequote `\'` of the char."))
        boolLit = BoolLit <$> (try (string "true" $> True) <|> try (string "false" $> False))
        fltLit = FloatLit <$> (try ((0 -) <$> (char '-' *> float)) <|> try float)
        intLit = IntLit <$> (try ((0 -) <$> (char '-' *> decimal)) <|> try decimal)
        strLit = StringLit <$> try (quote *> manyTill anySingle (quote <?> "closing quote `\"` of the string."))

        -- Symbol
        curlyOpenSym = symbol "{" $> CurlyOpen
        curlyCloseSym = symbol "}" $> CurlyClose
        parenOpenSym = symbol "(" $> ParenOpen
        parenCloseSym = symbol ")" $> ParenClose
        bracketOpenSym = symbol "[" $> BracketOpen
        bracketCloseSym = symbol "]" $> BracketClose
        assignSym = symbol "=" $> Assign
        arrowSym = symbol "->" $> Arrow
        scopeSym = symbol "." $> Scope
        semicolonSym = char ';' $> SemiColon
        commaSym = char ',' $> Comma

        -- Keyword
        functionKw = try $ keyword "function" $> FunctionKw
        operatorKw = try $ keyword "operator" $> OperatorKw
        precedenceKw =  try $ keyword "precedence" $> PrecedenceKw
        structKw = try $ keyword "struct" $> StructKw
        isKw = try $ keyword "is" $> IsKw
        importKw =  try $ keyword "import" $> ImportKw
        asKw = try $ keyword "as" $> AsKw
        atKw = try $ keyword "at" $> AtKw
        ifKw = try $ keyword "if" $> IfKw
        elseKw = try $ keyword "else" $> ElseKw
        returnKw = try $ keyword "return" $> ReturnKw

        -- Type
        charT = try $ keyword "char" $> CharType
        voidT = try $ keyword "void" $> VoidType
        boolT = try $ keyword "bool" $> BoolType
        intT = try $ keyword "int" $> IntType
        floatT = try $ keyword "float" $> FloatType
        strT = try $ keyword "str" $> StrType
        arrT = try $ keyword "arr" $> ArrType

        -- Identifier
        symbolId = try $ SymbolId <$> some prefixIdentifierChar
        operatorId = try $ OperatorId <$> some infixIdentifierChar
-- TokenType
