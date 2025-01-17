{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Import
-}

module Ast.Import (resolveImports, errImport) where

import Control.Exception (IOException, catch)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)
import System.Directory (getCurrentDirectory)

import Ast.Ast
import Parser.Token
import Parser.Tokenizer
import Utils.Lib
import Data.Maybe (mapMaybe, fromJust)
import Utils.ArgParser
import Data.List (intercalate)
import Ast.Parser (tokenToAst)
import Text.Megaparsec (errorBundlePretty)
import Ast.Error (prettyPrintError, blue, colorblindMode)

errImport :: [String] -> String -> IO a
errImport pathTried file = do
    cwd <- getCurrentDirectory
    putStrLn (printf "couldn't import the file '%s'. tried: %s" file (intercalate ", " $ map ((cwd ++ "/") ++) pathTried)) >> exitWith (ExitFailure 1)

errInputFile :: String -> String -> IO a
errInputFile file err = putStrLn (printf "the import '%s' contains errors:\n%s\n" (blue file) err) >> exitWith (ExitFailure 1)

-- todo: should handle lib path
getContent :: [String] -> [String] -> String -> IO String
getContent pathTried [] file = errImport pathTried file
getContent pathTried (importPath: xs) file = catch (readFile $ printf "%s/%s.mary" importPath file) invalidFile
    where
        invalidFile :: IOException -> IO String
        invalidFile _ = getContent (importPath:pathTried) xs file

importLib :: Arguments -> Ctx -> Ctx -> [String] -> [String] -> IO ([String], Ctx)
importLib _ _ ctx importedLib [] = pure (importedLib, ctx)
importLib args builtins ctx importedLib (libname:xs)
    | libname `elem` importedLib = pure (importedLib, ctx)
    | otherwise = do
        content <- getContent [] (argImportPath args) libname
        case run tokenize content of
            Left tokErr -> errInputFile libname $ errorBundlePretty tokErr
            Right (pos, tokens) -> do
                (importedLib', ctx') <- importLib args builtins ctx importedLib (findImports tokens)
                case run (tokenToAst builtins ctx') tokens of
                    Left astErr -> errInputFile libname $ (if argColorblind args then colorblindMode else id) $ prettyPrintError (fromJust $ argInputFile args) pos tokens astErr
                    Right ctx'' -> importLib args builtins ctx'' (importedLib' ++ [libname]) xs

isImportKw :: MyToken -> Maybe String
isImportKw (ImportKw importPath) = Just importPath
isImportKw _                     = Nothing

findImports :: [MyToken] -> [String]
findImports = mapMaybe isImportKw

importBuiltins :: Arguments -> IO Ctx
importBuiltins args = snd <$> importLib args [] [] [] ["builtins"]

-- (builtins, other imports)
resolveImports :: Arguments -> [MyToken] -> IO (Ctx, Ctx)
resolveImports args tokens = do
    builtins <- if argImportBuiltins args
        then importBuiltins args
        else pure []
    (_, ctx) <- importLib args builtins [] ["builtins"] (findImports tokens)
    return (builtins, ctx)
