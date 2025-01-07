{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ArgParser
-}

module ArgParser (parseArguments, Arguments(..), OutputType(..)) where


import Utils.Lib (run)

import Text.Megaparsec (Parsec, single, choice, eof, anySingle)
import Text.Megaparsec.Error (ParseErrorBundle)

import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative (empty, (<|>))


data OutputType = OutputType
    { tokenTy :: Bool
    , astTy :: Bool
    , bytecodeTy :: Bool
    }

data Arguments = Arguments
    { outputType :: OutputType
    , importPath :: [String]
    , optimize :: Bool
    , inputFile :: Maybe String
    , showHelper :: Bool
    }

type Parser = Parsec Void [String]

defaultArguments :: Arguments
defaultArguments = Arguments
    { outputType = OutputType
        { tokenTy = False
        , astTy = False
        , bytecodeTy = False
        }
    , importPath = []
    , optimize = False
    , inputFile = Nothing
    , showHelper = False
    }

flag :: String -> Parser String
flag = single

pOutputType :: Arguments -> Parser Arguments
pOutputType args = choice
    [ flag "--token"    $> args { outputType = (outputType args) { tokenTy = True } }
    , flag "--ast"      $> args { outputType = (outputType args) { astTy = True } }
    , flag "--bytecode" $> args { outputType = (outputType args) { bytecodeTy = True } }
    ]

pImportPath :: Arguments -> Parser Arguments
pImportPath args = (flag "-i" <|> flag "--import") *> ((\i -> args { importPath = i : importPath args }) <$> anySingle)

pOptimize :: Arguments -> Parser Arguments
pOptimize args = (flag "-o" <|> flag "--optimize") $> args {optimize = True}

pHelper :: Arguments -> Parser Arguments
pHelper args = (flag "--help" <|> flag "-h") $> args {showHelper = True}

pInput :: Arguments -> Parser Arguments
pInput args = (\i -> args {inputFile = i}) . Just <$> anySingle

pArguments :: Arguments -> Parser Arguments
pArguments args = eof $> args <|> (pOneArgument >>= pArguments)
    where pOneArgument = foldr ((<|>) . \f -> f args) empty
            [ pOutputType
            , pImportPath
            , pOptimize
            , pHelper
            , pInput
            ]

parseArguments :: [String] -> Either (ParseErrorBundle [String] Void) Arguments
parseArguments = run (pArguments defaultArguments)
