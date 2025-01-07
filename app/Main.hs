{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..), exitSuccess)

import Control.Exception (IOException, catch)

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)

import Text.Megaparsec (errorBundlePretty)


import Utils.Lib ((&>), run)
import ArgParser (parseArguments, Arguments (..), OutputType (..))

import Parser.Tokenizer (tokenize, comment)
import Ast.Ast (tokenToAst)
import Ast.Error (prettyPrintError, bgBlack)
import Bytecode.Compiler (compiler)
import VM.VirtualMachine (exec)


glados :: Arguments -> String ->  IO ()
glados args = toToken
    where
        toToken input = case run (comment &> tokenize) input of
            Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 1)
            Right tokens
                | tokenTy $ outputType args -> putStrLn $ intercalate "  " (map (bgBlack . show) tokens)
                | otherwise -> toAst tokens

        toAst tokens = case run tokenToAst tokens of
            Left err -> putStrLn (prettyPrintError tokens err) >> exitWith (ExitFailure 1)
            Right ast
                | astTy $ outputType args -> print ast
                | otherwise -> toBytecode ast
        
        toBytecode ast = case compiler ast of
            Left err -> print err >> exitWith (ExitFailure 1)
            Right (instr, env)
                | bytecodeTy $ outputType args -> print env
                | otherwise -> runVm env instr
        
        runVm env instr = case exec env instr [] of
            Left err -> print err >> exitWith (ExitFailure 1)
            Right result -> print result


helper :: String
helper =
    "execute the .mary file.\n" ++
    "\n" ++
    "<file>             => mandatory: an input file.\n" ++
    "\n" ++
    "--help             => display the helper.\n" ++
    "--token            => display the tokens.\n" ++
    "--ast              => display the ast.\n" ++
    "--bytecode         => display the bytecode.\n" ++
    "--import <path>    => import path for the libraries.\n" ++
    "--optimize         => optimize the code before running it.\n"


handleArgs :: Arguments -> IO ()
handleArgs args
    | showHelper args = putStrLn helper >> exitSuccess
    | isNothing $ inputFile args = putStrLn helper >> exitWith (ExitFailure 1)
    | otherwise = getFile (fromJust $ inputFile args) >>= \input -> glados args input
    where
        getFile file = catch (readFile file) invalidFile

        invalidFile :: IOException -> IO String
        invalidFile _ = putStrLn "Invalid input file." >> exitWith (ExitFailure 1)


main :: IO ()
main = getArgs >>= \args -> either (\_ -> putStrLn helper >> exitWith (ExitFailure 1)) handleArgs (parseArguments args)
