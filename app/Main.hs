{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Main (main) where

import System.Environment (getArgs)

import Lib (handleArgs)

main :: IO ()
main = getArgs >>= handleArgs
