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
import Data.List (intercalate)

import Text.Megaparsec (errorBundlePretty)

import Utils.Lib ((&>), run)
import Parser.Tokenizer (tokenize, comment)

import Ast.Ast (tokenToAst)
import Ast.Error (prettyPrintError, bggray)

import Bytecode.Compiler (compiler)
import Bytecode.Display (displayBytecode)
import VM.VirtualMachine (exec)
type Error = String

data Depth = Token | Ast | ByteCode deriving (Eq)

glados :: (Maybe Depth, String) -> IO ()
glados (d, content) = case run (comment &> tokenize) content of
  Left tokErr -> putStrLn $ errorBundlePretty tokErr
  Right tokens
    | d == Just Token -> putStrLn $ intercalate "  " (map (bggray . show) tokens)
    | otherwise -> case run tokenToAst tokens of
    Left astErr -> putStrLn $ prettyPrintError tokens astErr
    Right ast
        | d == Just Ast -> print ast
        | otherwise -> case compiler ast of
            Left errBytecode -> print errBytecode
            Right (_instr, env)
                | d == Just ByteCode -> displayBytecode _instr env
                | otherwise -> print $ exec env _instr []

helper :: String
helper =
    "execute the file given as argument.\n" ++
    "--help       => help.\n" ++
    "--token      => generate the tokens.\n" ++
    "--ast        => generate the ast.\n" ++
    "--bytecode   => generate bytecode."


handleArgs :: [String] -> IO (Either Error (Maybe Depth, String))
handleArgs x = do
    case x of
        ["--token",    file] -> getFile file $ Just Token
        ["--ast",      file] -> getFile file $ Just Ast
        ["--bytecode", file] -> getFile file $ Just ByteCode
        [file]               -> getFile file   Nothing
        _ -> return $ Left helper
    where
        getFile file depth = catch (Right . (,) depth <$> readFile file) invalidFile

        invalidFile :: IOException -> IO (Either Error (Maybe Depth, String))
        invalidFile _ = return $ Left "Invalid file."


main :: IO ()
main = getArgs >>= handleArgs >>= either (\err -> putStrLn err >> exitWith (ExitFailure 1)) glados
