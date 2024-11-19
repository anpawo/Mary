{-
-- EPITECH PROJECT, 2024
-- SExprParser.hs
-- File description:
-- mypandoc
-}

module SExprParser
    ( parseSExpr,
    parseSExprList
    ) where

import Parser
import Control.Applicative

data SExpr = SExprAtomInt Int | SExprAtomString String | SExprList [SExpr] deriving (Show)

parseSExprAtomInt :: Parser SExpr
parseSExprAtomInt = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser parseUInt rest of
            Right (num, rest1) -> 
                case runParser (parseAnyChar ['a'..'z'] <|> parseAnyChar ['A'..'Z']) rest1 of
                    Right (_, _) -> Left "Fail"
                    Left _ -> Right (SExprAtomInt num, rest1)
            Left _ -> Left "Fail"
        Left _ -> Left "Fail"

parseSExprAtomString :: Parser SExpr
parseSExprAtomString = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseMany (parseNotTheseChars " )")) rest of
            Right (str, rest1) -> Right (SExprAtomString str, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseSExprList :: Parser SExpr
parseSExprList = parseChar '(' >> parseMany (parseAnyChar " \t\n") >>
    ((:) <$> parseSExpr <*>
    parseMany (parseAnyChar " \t\n" *>
    parseMany (parseAnyChar " \t\n") *> parseSExpr)
    <|> pure []) >>= \values ->
    parseMany (parseAnyChar " \t\n")
    >> parseChar ')' >> return (SExprList values)

parseSExpr :: Parser SExpr
parseSExpr = parseSExprList <|> parseSExprAtomInt <|> parseSExprAtomString


