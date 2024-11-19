{-
-- EPITECH PROJECT, 2024
-- Parser.hs
-- File description:
-- glados
-}

module Parser
    ( Parser(..),
    parseWord,
    parseMany,
    parseChar,
    parseUInt,
    parseAnyChar,
    parseString,
    parseInt,
    parseNotThisChar,
    parseNotTheseChars,
    parseOr,
    parseAnd,
    parseAndWith,
    parseSome,
    parsePrefix
    ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Data.List (isPrefixOf)

data Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap f (Parser fc) = Parser $ \str -> case fc str of
        Right (res, str1) -> Right (f res, str1)
        Left err -> Left err

instance Applicative Parser where
    pure x = Parser $ \str -> Right (x, str)
    (Parser fc1) <*> (Parser fc2) = Parser $ \str -> case fc1 str of
        Right (res1, str1) -> case fc2 str1 of
            Right (res2, str2) -> Right (res1 res2, str2)
            Left err -> Left err
        Left err -> Left err
    (Parser fc1) <* (Parser fc2) = Parser $ \str -> case fc1 str of
        Right (res1, str1) -> case fc2 str1 of
            Right (_, str2) -> Right (res1, str2)
            Left err -> Left err
        Left err -> Left err
    (Parser fc1) *> (Parser fc2) = Parser $ \str -> case fc1 str of
        Right (_, str1) -> case fc2 str1 of
            Right (res2, str2) -> Right (res2, str2)
            Left err -> Left err
        Left err -> Left err

instance Alternative Parser where
    empty = Parser $ \_ -> Left "Empty parser"
    (Parser fc1) <|> (Parser fc2) = Parser $ \str -> case fc1 str of
        Right res -> Right res
        _ -> fc2 str

instance Monad Parser where
    return = pure
    (Parser fc) >>= f = Parser $ \str -> case fc str of
        Right (res, str1) -> runParser (f res) str1
        Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c = Parser fc where
    fc [] = Left "Empty string"
    fc (x:xs)
        | c == x = Right (x, xs)
        | otherwise = Left (c:" not found")

parseNotThisChar :: Char -> Parser Char
parseNotThisChar c = Parser fc where
    fc [] = Left "Empty string"
    fc (x:xs)
        | c /= x = Right (x, xs)
        | otherwise = Left (c:" found")

parseNotTheseChars :: String -> Parser Char
parseNotTheseChars [] = Parser $ \_ -> Left "Empty string"
parseNotTheseChars str = Parser fc where
    fc [] = Left "Empty string"
    fc (x:xs)
        | True `notElem` find = Right (x, xs)
        | otherwise = Left (str ++ " found")
        where
            find = map (\c -> c == x) str

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ \_ -> Left "Empty string"
parseAnyChar str = Parser fc where
    fc [] = Left "Empty string"
    fc (x:xs)
        | True `elem` find = Right (x, xs)
        | otherwise = Left (str ++ " not found")
        where
            find = map (== x) str

parseOr :: Parser a -> Parser a -> Parser a
parseOr fc1 fc2 = fc1 <|> fc2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd fc1 fc2 = (,) <$> fc1 <*> fc2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f fc1 fc2 = f <$> fc1 <*> fc2

parseMany :: Parser a -> Parser [a]
parseMany ogfc = Parser fc where
    fc str = case runParser ogfc str of
        Right (res1, str1) -> case runParser (parseMany ogfc) str1 of
            Right (res2, str2) -> Right (res1 : res2, str2)
            Left _ -> Right ([res1], str1)
        Left _ -> Right ([], str)

parseSome :: Parser a -> Parser [a]
parseSome ogfc = Parser fc where
    fc str = case runParser (parseAnd ogfc (parseMany ogfc)) str of
        Right ((res1, res2), str1) -> Right (res1 : res2, str1)
        Left _ -> Left "Fail"

parseUInt :: Parser Int
parseUInt = Parser fc where
    fc str = case runParser (parseSome (parseAnyChar ['0'..'9'])) str of
        Right (res, str1) -> Right (read res, str1)
        Left _ -> Left "Fail"

parseInt :: Parser Int
parseInt = Parser fc where
    fc str = case runParser (parseAndWith
            (\x y -> read x * y) (fmap (:[]) (parseChar '-')) parseUInt) str of
        Right (res, str1) -> Right (res, str1)
        Left _ -> case runParser parseUInt str of
            Right (res, str1) -> Right (res, str1)
            Left _ -> Left "Parse int failed"

parseWord :: String -> Parser String
parseWord word = Parser fc where
    fc str = case runParser (parseMany (parseAnyChar word)) str of
        Right (res, str1) -> if res == word
                             then Right (res, str1)
                             else Left "Word not found"
        Left err -> Left err

parseString :: Parser String
parseString = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseChar '"' *>
            parseMany (parseNotThisChar '"') <* parseChar '"') rest of
            Right (str, rest1) -> Right (str, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parsePrefix :: String -> Parser String
parsePrefix prefix = Parser fc where
    fc str
        | isPrefixOf prefix str = Right (prefix, drop (length prefix) str)
        | otherwise = Left "Fail"
