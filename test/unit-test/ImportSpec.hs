{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ImportSpec
-}

module ImportSpec (spec) where

import Ast.Import (errImport)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import System.IO.Silently (capture_, capture)
import System.Exit (ExitCode(..))
import Control.Exception (catch)

spec :: Spec
spec = do
  errImportSpec

-- failMsg :: IOException -> IO (Either String a)
-- failMsg (IOError {..}) = return $ Left ioe_description

exitError :: ExitCode -> IO (Either Int a)
exitError (ExitFailure n) = return $ Left n
exitError ExitSuccess = return $ Left 0

errImportSpec :: SpecWith ()
errImportSpec = describe "err import" $ do
  it "err import exit code" $ do
    x <- errImport [] "" `catch` exitError
    (x :: Either Int String) `shouldBe` Left 1
