{-
-- EPITECH PROJECT, 2024
-- SExprParser.hs
-- File description:
-- mypandoc
-}

module SExprParser
    ( parseSExpr
    ) where

import Parser
import Control.Applicative

data SExpr = SExprAtomInt Int | SExprAtomString String | SExprString String | SExprList [SExpr] deriving (Show)

parseSExprAtomInt :: Parser SExpr
parseSExprAtomInt = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseUInt <* parseNotTheseChars ['a'..'z']) rest of
            Right (num, rest1) -> Right (SExprAtomInt num, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseSExprAtomString :: Parser SExpr
parseSExprAtomString = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseMany (parseNotThisChar ' ')) rest of
            Right (str, rest1) -> Right (SExprAtomString str, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseSExprString :: Parser SExpr
parseSExprString = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser parseString rest of
            Right (str, rest1) -> Right (SExprString str, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseSExpr :: Parser SExpr
parseSExpr = parseSExprAtomInt <|> parseSExprString <|> parseSExprAtomString


