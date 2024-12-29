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
    namespace,
    tokenize,

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

import Text.Megaparsec (Parsec, many, manyTill, anySingle, eof, parseTest, manyTill_, (<?>), oneOf, notFollowedBy, try, someTill)
import Text.Megaparsec.Char (char, string, alphaNumChar, asciiChar)
import Text.Megaparsec.Char.Lexer (decimal, float)
import Text.Megaparsec.Debug (dbg)

import Parser.Token
import Utils.Lib (choicetry, (~>))

type Parser = Parsec Void String

-- utils
skipString :: Parser String
skipString = (\s -> '"' : s ++ "\"" ) <$> (quote *> manyTill anySingle (quote <?> "closing quote `\"` of the string."))

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
infixIdentifierChar = oneOf ['+', '-', '*', '/', '<', '>', '|', '^', '&', '~', '!', '$']

underscore :: Parser Char
underscore = char '_'

spaces :: Parser String
spaces = many $ oneOf " \n\t"

linespaces :: Parser String
linespaces = some $ oneOf " \t"
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
tokenize :: Parser [MyToken]
tokenize = spaces *> manyTill (tokens <* spaces) eof
    where
        tokens = choicetry
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
            , Type <$> parseType

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
        curlyOpenSym = char '{' $> CurlyOpen
        curlyCloseSym = char '}' $> CurlyClose
        parenOpenSym = char '(' $> ParenOpen
        parenCloseSym = char ')' $> ParenClose
        bracketOpenSym = char '[' $> BracketOpen
        bracketCloseSym = char ']' $> BracketClose
        assignSym = char '=' $> Assign
        arrowSym = try $ symbol "->" $> Arrow
        scopeSym = char '.' $> Scope
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

        parseType = choicetry [
            -- Type
              keyword "char" $> CharType
            , keyword "void" $> VoidType
            , keyword "bool" $> BoolType
            , keyword "int" $> IntType
            , keyword "float" $> FloatType
            , keyword "str" $> StrType
            , keyword "arr" *> do
                void $ spaces *> char '[' 
                t <- spaces *> parseType
                void $ spaces *> char ']'
                return $ ArrType t
            ]

        -- Identifier
        symbolId = SymbolId <$> some prefixIdentifierChar -- thought i prevented numbers as starting names
        operatorId = OperatorId <$> some infixIdentifierChar
-- TokenType
