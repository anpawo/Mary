{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, ExitCode (ExitFailure))
import System.Environment (getArgs)
import Control.Exception (IOException, catch)

import Text.Megaparsec (errorBundlePretty)

import Utils.Lib ((&>), run)
import Parser.Tokenizer (tokenize, comment)

import Ast.Ast (tokenToAst)
import Ast.Error (prettyPrintError)

import Bytecode.Compiler (compiler)

type Error = String

data Depth = Token | Ast | ByteCode deriving (Eq)

glados :: (Maybe Depth, String) -> IO ()
glados (d, content) = case run (comment &> tokenize) content of
  Left err -> putStrLn $ errorBundlePretty err
  Right tokens
    | d == Just Token -> print tokens
    | otherwise -> case run tokenToAst tokens of
    Left err' -> putStrLn $ prettyPrintError tokens err'
    Right ast
        | d == Just Ast -> print ast
        | otherwise -> case compiler ast of
            Left errBytecode -> print errBytecode
            Right (_instr, env)
                | d == Just ByteCode -> print env
                | otherwise -> print "vm todo"


helper :: String
helper =
    "execute the file given as argument.\n" ++
    "--help       => help.\n" ++
    "--token      => generate the tokens.\n" ++
    "--ast        => generate the ast.\n" ++
    "--bytecode   => generate bytecode."


handleArgs :: [String] -> IO (Either Error (Maybe Depth, String))
handleArgs ["--token", file] = do
    x <- handleArgs [file]
    pure ((,) (Just Token) . snd <$> x)
handleArgs ["--ast", file] = do
    x <- handleArgs [file]
    pure ((,) (Just Ast) . snd <$> x)
handleArgs ["--bytecode", file] = do
    x <- handleArgs [file]
    pure ((,) (Just ByteCode) . snd <$> x)
handleArgs [file] = catch (Right . (,) Nothing <$> readFile file) invalidFile
    where
        invalidFile :: IOException -> IO (Either Error (Maybe Depth, String))
        invalidFile _ = return $ Left "Invalid file."
handleArgs _ = return $ Left helper


main :: IO ()
main = getArgs >>= handleArgs >>= either (\err -> putStrLn err >> exitWith (ExitFailure 84)) glados
