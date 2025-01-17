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

import Opti.Optimizer (optimizeAST)
import Utils.Lib (run)
import Utils.ArgParser (parseArguments, Arguments (..), OutputType (..))
import Parser.Tokenizer (tokenize)
import Ast.Parser (tokenToAst)
import Ast.Error (prettyPrintError, bgBlack, colorblindMode)
import Bytecode.Compiler (compiler)
import Bytecode.Display (displayBytecode)
import VM.VirtualMachine (exec)
import Ast.Import (resolveImports)
import Control.Monad (void)
import System.Directory (makeAbsolute)

import Ast.HandleArg (handleArgs, helper, glados)

main :: IO ()
main = getArgs >>= \args -> either (\_ -> putStrLn helper >> exitWith (ExitFailure 1)) handleArgs (parseArguments args)
