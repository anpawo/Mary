{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib
  ( glados,
  )
where

import AST.Data ( AST(AstDefine), Define )
-- import AST.Sexpression ( sexprToAST )
import Control.Exception (IOException, catch)
import System.Environment (getArgs)
import Parser.Tokenizer ( run, tokenize )
import AST.Conv ( tokenToAST )
import Text.Megaparsec (errorBundlePretty)
import Parser.SortToken ( splitBySemicolon, sortToken )

-- parseToAST :: [Define] -> String -> Either String ([Define], AST)
-- parseToAST list_define content = case runParser parseSExpr content of
--   Left err -> Left $ "Parsing error: " ++ err
--   Right (sexpr, _) -> case sexprToAST sexpr >>= evalAST list_define of
--     Left err -> Left $ "Error: " ++ err
--     Right res -> Right res

-- gladosRepl :: [Define] -> IO ()
-- gladosRepl list_define = do
--   putStr "> "
--   input <- getLine
--   if input == "quit"
--     then return ()
--     else do
--         case parseToAST list_define input of
--             Left err -> do
--                 putStrLn err
--                 gladosRepl list_define
--             Right (new_list_define, value) -> do
--                 valuePrint value
--                 gladosRepl new_list_define

-- valuePrint :: AST -> IO()
-- valuePrint value = case value of
--   AstDefine _ -> return ()
--   _ -> print value

compileStdin :: [Define] -> String -> IO ()
compileStdin list_define content = case run tokenize content of
  Left err -> print (errorBundlePretty err)
  Right res -> print (sortToken res)
    -- case sequence (map tokenToAST res) of
    -- Right asts -> print asts
    -- Left err -> putStrLn $ "Error: " ++ err

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

processArgs :: [String] -> IO (Either String String)
processArgs args = case args of
  ["-h"] -> do
    printUsage
    return $ Left "End of the help text"
  ["-f", filePath] -> readFileEither filePath
  [] -> do Right <$> getContents
  -- ["-repl"] -> do
  --   putStrLn "Welcome to GLaDOS REPL. Type 'quit' to exit."
  --   gladosRepl []
  --   return $ Left "REPL mode exited."
  [expression] -> return $ Right expression
  _ -> do
    printUsage
    return $ Left "Invalid arguments."

glados :: IO ()
glados = do
  args <- getArgs
  result <- processArgs args
  case result of
    Left err -> putStrLn err
    Right content -> compileStdin [] content