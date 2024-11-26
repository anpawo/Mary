{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}

module Lib
    ( glados
    ) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import SExprParser()
import Text.Printf (printf)

-- todo changer la verification des arguments, le cas où il n'y a pas d'arguments ne peux pas lire directement stdin mais doit appeler la fonction SExprtoAST et lui "dire" de lire stdin

glados :: IO ()
glados = do
    args <- getArgs
    result <- processArgs args
    case result of
        Left err -> do
            putStrLn err
            exitWith (ExitFailure 1)
        Right content -> putStrLn content -- use content as we want

processArgs :: [String] -> IO (Either String String)
processArgs args = case args of
    ["-f", filePath] -> readFileEither filePath
    ["--file", filePath] -> readFileEither filePath
    -- [x] -> --todo appeler SexprtoAST ou la fonction qu'il faut avec x en paramètre
    [] -> readStdin
    _ -> do
        printUsage
        return $ Left "Invalid arguments."

readFileEither :: FilePath -> IO (Either String String)
readFileEither filePath = catch (Right <$> readFile filePath) handleReadError
  where
    handleReadError :: IOException -> IO (Either String String)
    handleReadError _ = return $ Left "Error reading file"

readStdin :: IO (Either String String)
readStdin = Right <$> getContents

printUsage :: IO()
printUsage = putStrLn "Usage: \n\tIf you want to evaluate a given expression:\n\t\t./glados your_expression\n\tIf you want to evaluate a given file:\n\t\t./glados [-f or --file] <file_name>\n\tIf you want to enter a REPL:\n\t\t./glados"