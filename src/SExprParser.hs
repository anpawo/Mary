{-
-- EPITECH PROJECT, 2024
-- SExprParser.hs
-- File description:
-- glados
-}

module SExprParser
    ( parseSExpr,
    printTree,
    parseSExprAtomInt,
    parseSExprAtomString,
    parseSExprList,
    SExpr(SExprAtomInt, SExprAtomString, SExprList)
    ) where

import Parser
    ( Parser(..),
      parseChar,
      parseNotTheseChars,
      parseAnyChar,
      parseMany,
      parseUInt )
import Control.Applicative ( Alternative((<|>)) )

data SExpr = SExprAtomInt Int | SExprAtomString String | SExprList [SExpr] deriving (Show)

instance Eq SExpr where
    (SExprAtomInt x) == (SExprAtomInt y) = x == y
    (SExprAtomString str1) == (SExprAtomString str2) = str1 == str2
    (SExprList list1) == (SExprList list2) = list1 == list2
    _ == _ = False

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
        Right (_, rest) -> case runParser (parseMany (parseNotTheseChars " \n\t)")) rest of
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
parseSExpr = Parser fc where
    fc str = case runParser (parseMany (parseAnyChar " \t\n")) str of
        Right (_, rest) -> runParser (parseSExprList <|> parseSExprAtomInt <|> parseSExprAtomString) rest
        Left _  -> Left "Fail"

getSymbol :: SExpr -> Maybe String
getSymbol (SExprAtomString s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SExprAtomInt s) = Just s
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SExprList s) = Just s
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SExprAtomInt n) = Just $ "a Number " ++ show n
printTree (SExprAtomString s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (SExprList sx) = do
    elements <- mapM printTree sx
    Just $ "a List with " ++ unwords elements