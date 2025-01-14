{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Lib
-}

module Utils.Lib (choicetry, (&>), (~>), run, failN, failP, failI, (!?)) where

import Text.Megaparsec (MonadParsec (updateParserState), try, Parsec, setInput, runParser, setOffset, getOffset, PosState(..), State(..), initialPos)
import Text.Megaparsec.Error (ParseErrorBundle)

import Control.Applicative ((<|>), empty)

import Data.Void (Void)

-- choice fusion with try
choicetry :: (Foldable f, MonadParsec e s m) => f (m a) -> m a
choicetry = foldr ((<|>) . try) empty

type Parser = Parsec Void String

-- combine p1 and p2 while updating the input. p2 needs a from (p1 => (a, b))
(~>) :: Parser (a, String) -> (a -> Parser b) -> Parser b
(~>) p1 p2 = p1 >>= (\(out, input) -> setInput input >> p2 out)

-- combine p1 and p2 while updating the input
(&>) :: Parser String -> Parser a -> Parser a
(&>) p1 p2 =
    p1 >>= setInput
    >> updateParserState (\(State s _ p e) -> State s 0 (p { pstateSourcePos = initialPos "" }) e)
    >> p2

-- runParser alias without context
run :: Parsec Void input output -> input -> Either (ParseErrorBundle input Void) output
run parser = runParser parser ""

-- fail at next token
failN :: (MonadParsec e s m, MonadFail m) => String -> m a
failN err = (setOffset . (+ 1) =<< getOffset) *> fail err

-- fail at prev token
failP :: (MonadParsec e s m, MonadFail m) => String -> m a
failP err = (setOffset . subtract 1 =<< getOffset) *> fail err

-- fail at index token
failI :: (MonadParsec e s m, MonadFail m) => Int -> String -> m a
failI idx err = setOffset idx *> fail err

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
