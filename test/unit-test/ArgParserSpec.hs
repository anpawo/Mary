{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module ArgParserSpec (spec) where

import Test.Hspec
import Utils.ArgParser

instance Eq OutputType where
  (==) o1 o2 =
    tokenTy o1 == tokenTy o2 &&
    astTy o1 == astTy o2 &&
    bytecodeTy o1 == bytecodeTy o2

instance Show OutputType where
  show o =
    "OutputType { tokenTy=" ++ show (tokenTy o)
    ++ ", astTy=" ++ show (astTy o)
    ++ ", bytecodeTy=" ++ show (bytecodeTy o)
    ++ "}"