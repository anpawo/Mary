{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Import
-}

module Ast.Import (resolveImports) where

import Control.Exception (IOException, catch)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)
import System.Directory (getCurrentDirectory)

import Ast.Ast
import Parser.Token
import Parser.Tokenizer
import Utils.Lib
import Data.Maybe (mapMaybe)
import Utils.ArgParser
import Data.List (intercalate)

errImport :: [String] -> String -> IO a
errImport pathTried file = do
    cwd <- getCurrentDirectory
    putStrLn (printf "couldn't import the file '%s'. tried: " file (intercalate ", " $ map ((cwd ++ "/") ++) pathTried)) >> exitWith (ExitFailure 1)

errInputFile :: String -> IO a
errInputFile file = putStrLn (printf "the file '%s' contains errors." file) >> exitWith (ExitFailure 1)

-- todo: should handle lib path
getContent :: [String] -> [String] -> String -> IO String
getContent pathTried [] file = errImport pathTried file
getContent pathTried (importPath: xs) file = catch (readFile $ printf "%s/%s.mary" importPath file) invalidFile
    where
        invalidFile :: IOException -> IO String
        invalidFile _ = getContent (importPath:pathTried) xs file

importLib :: Arguments -> Ctx -> Ctx -> String -> IO Ctx
importLib args builtins imports libname = do
    content <- getContent [] (argImportPath args) libname
    case run (comment &> tokenize) content of
        Left _ -> errInputFile libname
        Right tokens -> do
            libImports <- importOtherLib args builtins imports (findImports tokens)
            case run (tokenToAst builtins imports) tokens of
                Left _ -> errInputFile libname
                Right newctx -> pure (libImports ++ newctx)

isImportKw :: MyToken -> Maybe String
isImportKw (ImportKw importPath) = Just importPath
isImportKw _                     = Nothing

findImports :: [MyToken] -> [String]
findImports = mapMaybe isImportKw

importBuiltins :: Arguments -> IO Ctx
importBuiltins args = importLib args [] [] "builtins"

importOtherLib :: Arguments -> Ctx -> Ctx -> [String] -> IO Ctx
importOtherLib _ _ ctx [] = pure ctx
importOtherLib args builtins ctx (x:xs) = importLib args builtins ctx x >>= \newctx -> importOtherLib args builtins (ctx ++ newctx) xs

-- builtins - others
resolveImports :: Arguments -> [MyToken] -> IO (Ctx, Ctx)
resolveImports args tokens = do
    builtins <- if argImportBuiltins args
        then importBuiltins args
        else pure []
    otherImports <- importOtherLib args builtins [] (findImports tokens)
    return (builtins, otherImports)
