{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- glados
-}

module Lib
    ( glados
    ) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Applicative ( Alternative((<|>), empty) )
import SExprParser
import System.Environment
import System.IO

glados :: IO ()
glados = putStrLn "Glados"