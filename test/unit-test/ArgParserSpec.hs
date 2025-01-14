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

instance Eq Arguments where
  (==) a1 a2 =
    argOutputType a1 == argOutputType a2 &&
    argImportPath a1 == argImportPath a2 &&
    argOptimize a1 == argOptimize a2 &&
    argInputFile a1 == argInputFile a2 &&
    argShowHelper a1 == argShowHelper a2 &&
    argColorblind a1 == argColorblind a2 &&
    argImportBuiltins a1 == argImportBuiltins a2

instance Show Arguments where
  show a =
    "Arguments { argOutputType=" ++ show (argOutputType a)
    ++ ", argImportPath=" ++ show (argImportPath a)
    ++ ", argOptimize=" ++ show (argOptimize a)
    ++ ", argInputFile=" ++ show (argInputFile a)
    ++ ", argShowHelper=" ++ show (argShowHelper a)
    ++ ", argColorblind=" ++ show (argColorblind a)
    ++ ", argImportBuiltins=" ++ show (argImportBuiltins a)
    ++ "}"