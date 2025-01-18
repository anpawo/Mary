{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import Utils.ArgParser (parseArguments)
import System.Exit (exitWith, ExitCode(..))
import Utils.HandleArg (handleArgs, helper)

main :: IO ()
main = getArgs >>= \args -> either (\_ -> putStrLn helper >> exitWith (ExitFailure 1)) handleArgs (parseArguments args)
