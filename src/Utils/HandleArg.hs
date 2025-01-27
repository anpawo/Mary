{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module provides utility functions for handling command-line arguments.
-}

module Utils.HandleArg (handleArgs, helper, glados) where

import Control.Monad (void)
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import Control.Exception (IOException, catch)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Text.Megaparsec (errorBundlePretty)

import Utils.Lib (run)
import Utils.ArgParser (Arguments (..), OutputType (..))
import Parser.Tokenizer (tokenize)
import Ast.Parser (tokenToAst)
import Ast.Error (prettyPrintError, bgBlack, colorblindMode)
import Bytecode.Compiler (compiler)
import Bytecode.Display (displayBytecode)
import VM.VirtualMachine (exec)
import Ast.Import (resolveImports)
import System.Directory (makeAbsolute)
import Opti.Optimizer (optimizeAST)
import Opti.Optimizer2 (optimizeBytecode)

glados :: Arguments -> String ->  IO ()
glados args = toToken
    where
        toToken input = case run tokenize input of
            Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 1)
            Right tokens
                | tokenTy $ argOutputType args -> putStrLn $ intercalate "  " (map (bgBlack . show) (snd tokens))
                | otherwise -> toAst tokens

        toAst (pos, tokens) = do
            (builtins, imports) <- resolveImports args tokens
            case run (tokenToAst builtins imports) tokens of
                Left err -> putStrLn ((if argColorblind args then colorblindMode else id) $ prettyPrintError (fromJust $ argInputFile args) pos tokens err) >> exitWith (ExitFailure 1)
                Right ast ->
                    let ast' = if argOptimize args then optimizeAST ast else ast
                    in if astTy (argOutputType args)
                        then print ast'
                        else toBytecode ast'

        toBytecode ast = case (\(i, e) -> if argOptimize2 args then (i, optimizeBytecode e) else (i, e)) <$> compiler ast of
            Left err -> print err >> exitWith (ExitFailure 1)
            Right (instr, env)
                | bytecodeTy $ argOutputType args -> displayBytecode instr env
                | otherwise -> runVm env instr

        runVm env instr = void $ exec 0 env instr []

helper :: String
helper =
    "execute the .mary file.\n" ++
    "\n" ++
    "<file>             => mandatory: an input file.\n" ++
    "\n" ++
    "--token            => display the tokens.\n" ++
    "--ast              => display the ast.\n" ++
    "--bytecode         => display the bytecode.\n" ++
    "--import <path>    => import path for the libraries.\n" ++
    "--optimize         => optimize the ast.\n" ++
    "--optimize2        => optimize the bytecode.\n" ++
    "--colorblind       => display colors according to most colorblinds.\n" ++
    "--no-builtins      => doesn't load the builtins.\n"

handleArgs :: Arguments -> IO ()
handleArgs args
    | argShowHelper args = putStrLn helper >> exitSuccess
    | isNothing $ argInputFile args = putStrLn helper >> exitWith (ExitFailure 1)
    | otherwise = getFile (fromJust $ argInputFile args) >>= \input ->
        makeAbsolute (fromJust $ argInputFile args) >>= \path ->
            glados args { argInputFile = Just path } input
  where
    getFile file = catch (readFile file) invalidFile
    invalidFile :: IOException -> IO String
    invalidFile _ = putStrLn "Invalid input file." >> exitWith (ExitFailure 1)