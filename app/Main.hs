{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import Control.Exception (IOException, catch)

import Text.Megaparsec (errorBundlePretty)

import Parser.Tokenizer (run, tokenize, (&>), comment)
import Ast.Ast (tokenToAst)
import Ast.Error

type ArgInfo = String

glados :: String -> IO ()
glados content = case run (comment &> tokenize) content of
  Left err -> putStrLn $ errorBundlePretty err
  Right tokens -> case run tokenToAst tokens of
    Left err' -> putStrLn $ prettyPrintError tokens err'
    Right res' -> print res'


helper :: String
helper =
    "execute the file given as argument.\n" ++
    "--help       => help.\n" ++
    "--bytecode   => generate bytecode but do not execute it."


handleArgs :: [String] -> IO (Either ArgInfo String)
handleArgs ["--bytecode"] = return $ Left "todo bytecode"
handleArgs [file] = catch (Right <$> readFile file) invalidFile
    where
        invalidFile :: IOException -> IO (Either ArgInfo String)
        invalidFile _ = return $ Left "Invalid file."
handleArgs _ = return $ Left helper


main :: IO ()
main = getArgs >>= handleArgs >>= either putStrLn glados
