{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Tokenizer
-}

module Tokenizer
    ( rmComments,
      -- tests
      t,
      Parser,
      runParser,
      parseTest,
      -- tests
    ) where

-- import Token

import Data.Void (Void)
import Data.List (singleton)
import Data.Functor (($>))
import Control.Applicative ((<|>))

import Text.Megaparsec (Parsec, many, noneOf, manyTill, anySingle, eof, parseTest, manyTill_, runParser, (<?>))
import Text.Megaparsec.Char (char, string)
-- import Text.Megaparsec.Char.Lexer (decimal, float, signed)

type Parser = Parsec Void String

-- used for tests
t :: Parser String -> String -> IO ()
t = parseTest
-- used for tests

rmComments :: Parser String
rmComments = concat <$> manyTill (skipString <|> singleLineComment <|> multiLineComment <|> (singleton <$> anySingle)) eof
    where
        skipString :: Parser String
        skipString = (\s -> '"' : s ++ "\"" ) <$> (char '"' *> many (noneOf "\"") <* (char '"' <?> "closing quote `\"` of the string.")) 

singleLineComment :: Parser String
singleLineComment =  snd <$> (startComment *> manyTill_ anySingle endComment)
    where
        startComment = string "//"
        endComment = string "\n" <|> eof $> ""

multiLineComment :: Parser String
multiLineComment = startComment *> manyTill anySingle endComment $> ""
    where
        startComment :: Parser String
        startComment = string "/*"
        
        endComment :: Parser String
        endComment = string "*/" <?> "end of multi-line comment \"*/\"."
