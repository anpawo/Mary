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
        Right content -> parseToAST content

gladosRepl :: IO ()
gladosRepl = do
    putStrLn "Welcome to GLaDOS REPL. Type ':quit' to exit."
    loop
  where
    loop = do
        putStr "> "
        input <- getLine
        if input == "quit" then return ()
        else do
            parseToAST input
            loop

parseToAST :: String -> IO()
parseToAST content = case runParser parseSExpr content of
                Left err -> putStrLn $ "Parsing error: " ++ err
                Right (sexpr, _) -> case sexprToAST sexpr >>= evalAST of
                    Left err -> putStrLn $ "Error: " ++ err
                    Right value -> print value

processArgs :: [String] -> IO (Either String String)
processArgs args = case args of
    ["-f", filePath] -> readFileEither filePath
    ["--file", filePath] -> readFileEither filePath
    [] -> do
        gladosRepl
        return $ Left "REPL mode exited."
    [expression] -> return $ Right expression
    _ -> do
        printUsage
        return $ Left "Invalid arguments."

readFileEither :: FilePath -> IO (Either String String)
readFileEither filePath = catch (Right <$> readFile filePath) handleReadError
  where
    handleReadError :: IOException -> IO (Either String String)
    handleReadError _ = return $ Left "Error reading file"

printUsage :: IO()
printUsage = putStrLn $ unlines
    [ "Usage: glados [OPTIONS] [EXPRESSION]"
    , ""
    , "Options:"
    , "  -f, --file <file>   Evaluate expressions from the given file."
    , "  --help              Show this help text."
    , ""
    , "Examples:"
    , "  ./glados \"(+ 1 2)\"     Evaluate the expression."
    , "  ./glados -f expr.txt   Evaluate expressions from 'expr.txt'."
    , "  ./glados              Start REPL mode."]
