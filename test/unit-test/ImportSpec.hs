{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module contains unit tests for the Import functionality.
-}

module ImportSpec (spec) where

import Ast.Import (errImport, errInputFile)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import System.Exit (ExitCode(..))
import Control.Exception (catch)

spec :: Spec
spec = do
  errImportSpec
  errInputFileSpec

exitError :: ExitCode -> IO (Either Int a)
exitError (ExitFailure n) = return $ Left n
exitError ExitSuccess = return $ Left 0

errImportSpec :: SpecWith ()
errImportSpec = describe "err import" $ do
  it "err import exit code" $ do
    x <- errImport [] "" `catch` exitError
    (x :: Either Int String) `shouldBe` Left 1

errInputFileSpec :: SpecWith ()
errInputFileSpec = describe "err input file" $ do
  it "err input file exit code" $ do
    x <- errInputFile [] "" `catch` exitError
    (x :: Either Int String) `shouldBe` Left 1
