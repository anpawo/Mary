{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}

module Lib (handleArgs) where

import Control.Exception (IOException, catch)
import System.Environment (getArgs)
import Parser.Tokenizer ( run, tokenize )
import AST.Conv ( tokenToAST )
import Text.Megaparsec (errorBundlePretty)
import Parser.SortToken ( sortToken )

import Control.Exception (catch, IOException)
import System.Exit (ExitCode(ExitFailure), exitWith)

glados :: String -> IO ()
glados content = case run tokenize content of
  Left err -> print (errorBundlePretty err)
  Right res -> case (sortToken res) of
    Right sort_list -> case sequence (map tokenToAST sort_list) of
        Right asts -> print asts
        Left err -> putStrLn $ "Error token to ast: " ++ err
    Left err -> putStrLn $ "Error sort token: " ++ err

helper :: IO ()
helper = do
    putStrLn "execute the file given as argument.\n"
    putStrLn "--help       => help."
    putStrLn "--bytecode   => generate bytecode but do not execute it.\n"
    

handleArgs :: [String] -> IO ()
handleArgs ["--help"] = helper
handleArgs ["--bytecode"] = putStrLn "todo bytecode"
handleArgs [file] = catch (readFile file >>= glados) invalidFile
    where
        invalidFile :: IOException -> IO ()
        invalidFile _ = putStrLn "Invalid file." >> exitWith (ExitFailure 84)
handleArgs _ = helper
