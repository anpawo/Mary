{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}

module Lib
    ( glados
    ) where

import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import SExprParser(parseSExpr)
import AST(sexprToAST, evalAST)
import Parser(runParser)

glados :: IO ()
glados = do
    args <- getArgs
    result <- processArgs args
    case result of
        Left err -> putStrLn err
        Right content ->
            case runParser parseSExpr content of
                Left err -> putStrLn $ "Parsing error: " ++ err
                Right (sexpr, _) -> case sexprToAST sexpr >>= evalAST of
                    Left err -> putStrLn $ "Error: " ++ err
                    Right value -> print value

processArgs :: [String] -> IO (Either String String)
processArgs args = case args of
    ["-f", filePath] -> readFileEither filePath
    ["--file", filePath] -> readFileEither filePath
    [] -> readStdin
    [expression] -> return $ Right expression
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