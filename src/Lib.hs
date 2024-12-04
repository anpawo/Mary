{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}

module Lib
  ( glados,
  )
where

import AST (AST(..),Define, evalAST, sexprToAST)
import Control.Exception (IOException, catch)
import Parser (runParser)
import SExprParser (parseSExpr)
import System.Environment (getArgs)

glados :: IO ()
glados = do
  args <- getArgs
  result <- processArgs args
  case result of
    Left err -> putStrLn err
    Right content -> parseToASTStdin [] content

gladosRepl :: [Define] -> IO ()
gladosRepl list_define = do
  putStr "> "
  input <- getLine
  if input == "quit"
    then return ()
    else do
        case parseToAST list_define input of
            Left err -> do
                putStrLn err
                gladosRepl list_define
            Right (new_list_define, value) -> do
                valuePrint value
                gladosRepl new_list_define

valuePrint :: AST -> IO()
valuePrint value = case value of
  AstDefine _ -> return ()
  _ -> print value

parseToAST :: [Define] -> String -> Either String ([Define], AST)
parseToAST list_define content = case runParser parseSExpr content of
  Left err -> Left $ "Parsing error: " ++ err
  Right (sexpr, _) -> case sexprToAST sexpr >>= evalAST list_define of
    Left err -> Left $ "Error: " ++ err
    Right res -> Right res

parseToASTStdin :: [Define] -> String -> IO ()
parseToASTStdin list_define content = case runParser parseSExpr content of
  Left err -> print $ "Parsing error: " ++ err
  Right (sexpr, "") -> case sexprToAST sexpr >>= evalAST list_define of
    Left err -> print $ "Error: " ++ err
    Right (new_list_define, value) -> print value
  Right (sexpr, rest) -> case sexprToAST sexpr >>= evalAST list_define of
    Left err -> print $ "Error: " ++ err
    Right (new_list_define, value) -> do
      valuePrint value
      parseToASTStdin new_list_define rest

processArgs :: [String] -> IO (Either String String)
processArgs args = case args of
  ["-h"] -> do
    printUsage
    return $ Left "End of the help text"
  ["-f", filePath] -> readFileEither filePath
  [] -> do Right <$> getContents
  ["-repl"] -> do
    putStrLn "Welcome to GLaDOS REPL. Type 'quit' to exit."
    gladosRepl []
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

printUsage :: IO ()
printUsage =
  putStrLn $
    unlines
      [ "Usage: glados [OPTIONS] [EXPRESSION]",
        "",
        "Options:",
        "  -f <file>   Evaluate expressions from the given file.",
        "  -repl        Start REPL mode.",
        "  -h             Show this help text.",
        "",
        "Examples:",
        "  ./glados \"(+ 1 2)\"     Evaluate the expression.",
        "  ./glados -f expr.txt   Evaluate expressions from 'expr.txt'.",
        "  ./glados -repl              Start REPL mode."
      ]
