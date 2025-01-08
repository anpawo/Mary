{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ArgParser
-}

module Utils.ArgParser (parseArguments, Arguments(..), OutputType(..), defaultArguments) where


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
    { argOutputType :: OutputType
    , argImportPath :: [String]
    , argOptimize :: Bool
    , argInputFile :: Maybe String
    , argShowHelper :: Bool
    , argColorblind :: Bool
    , argImportBuiltins :: Bool
    }

type Parser = Parsec Void [String]

defaultArguments :: Arguments
defaultArguments = Arguments
    { argOutputType = OutputType
        { tokenTy = False
        , astTy = False
        , bytecodeTy = False
        }
    , argImportPath = ["stdlib"]
    , argOptimize = False
    , argShowHelper = False
    , argColorblind = False
    , argImportBuiltins = True
    , argInputFile = Nothing
    }

flag :: String -> Parser String
flag = single

pOutputType :: Arguments -> Parser Arguments
pOutputType args = choice
    [ flag "--token"    $> args { argOutputType = (argOutputType args) { tokenTy = True } }
    , flag "--ast"      $> args { argOutputType = (argOutputType args) { astTy = True } }
    , flag "--bytecode" $> args { argOutputType = (argOutputType args) { bytecodeTy = True } }
    ]

pImportPath :: Arguments -> Parser Arguments
pImportPath args = flag "--import" *> ((\i -> args { argImportPath = i : argImportPath args }) <$> anySingle)

pOptimize :: Arguments -> Parser Arguments
pOptimize args = flag "--optimize" $> args {argOptimize = True}

pHelper :: Arguments -> Parser Arguments
pHelper args = flag "--help" $> args {argShowHelper = True}

pColorblind :: Arguments -> Parser Arguments
pColorblind args = flag "--colorblind" $> args {argColorblind = True}

pImportBuiltins :: Arguments -> Parser Arguments
pImportBuiltins args = flag "--no-builtins" $> args {argImportBuiltins = False}

pInput :: Arguments -> Parser Arguments
pInput args = (\i -> args {argInputFile = i}) . Just <$> anySingle

pArguments :: Arguments -> Parser Arguments
pArguments args = eof $> args <|> (pOneArgument >>= pArguments)
    where pOneArgument = foldr ((<|>) . \f -> f args) empty
            [ pOutputType
            , pImportPath
            , pOptimize
            , pHelper
            , pColorblind
            , pImportBuiltins
            , pInput
            ]

parseArguments :: [String] -> Either (ParseErrorBundle [String] Void) Arguments
parseArguments = run (pArguments defaultArguments)
