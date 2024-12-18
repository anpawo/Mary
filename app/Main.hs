{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Control.Exception (IOException, catch)

import Parser.Tokenizer (run, tokenize)
import Text.Megaparsec (errorBundlePretty)
import Ast.Ast (tokenToAst)

type ArgInfo = String

glados :: String -> IO ()
glados content = case run tokenize content of
  Left err -> putStrLn (errorBundlePretty err)
  Right res -> print $ tokenToAst res


helper :: String
helper =
    "execute the file given as argument.\n" ++
    "--help       => help.\n" ++
    "--bytecode   => generate bytecode but do not execute it."


handleArgs :: [String] -> IO (Either ArgInfo String)
handleArgs ["--bytecode"] = return $ Left "todo bytecode"
handleArgs [file] = catch (readFile file >>= pure . Right) invalidFile
    where
        invalidFile :: IOException -> IO (Either ArgInfo String)
        invalidFile _ = return $ Left "Invalid file."
handleArgs _ = return $ Left helper


main :: IO ()
main = getArgs >>= handleArgs >>= either putStrLn glados
