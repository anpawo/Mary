{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Lib
-}

module Utils.Lib (choicetry, (&>), (~>), run) where

import Text.Megaparsec (MonadParsec, try, Parsec, setInput, runParser)
import Text.Megaparsec.Error (ParseErrorBundle)

import Control.Applicative ((<|>), empty)

import Data.Void (Void)


choicetry :: (Foldable f, MonadParsec e s m) => f (m a) -> m a
choicetry = foldr ((<|>) . try) empty

type Parser = Parsec Void String

-- combine p1 and p2 while updating the input. p2 needs a from (p1 => (a, b))
(~>) :: Parser (a, String) -> (a -> Parser b) -> Parser b
(~>) p1 p2 = p1 >>= (\(out, input) -> setInput input >> p2 out)

-- combine p1 and p2 while updating the input
(&>) :: Parser String -> Parser a -> Parser a
(&>) p1 p2 = p1 >>= setInput >> p2

run :: Parsec Void input output -> input -> Either (ParseErrorBundle input Void) output
run parser = runParser parser ""
