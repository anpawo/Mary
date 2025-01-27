{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- This module contains unit tests for the library functions.
-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
module LibSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either (isLeft)
import Data.Void (Void)
import Control.Monad (void)

import Utils.Lib

spec :: Spec
spec = do
  functionSpec
  operatorSpec

functionSpec :: Spec
functionSpec = do
  describe "choicetry" $ do
    it "tries multiple parsers" $ do
      let p1 = string "hello" >> return "p1"
      let p2 = string "hi" >> return "p2"
      run (choicetry [p1,p2]) "hello" `shouldBe` Right "p1"
      run (choicetry [p1,p2]) "hi"    `shouldBe` Right "p2"
      (run (choicetry [p1,p2]) "x" :: Either (ParseErrorBundle String Void) String)
        `shouldSatisfy` isLeft

  describe "run" $ do
    it "parses successfully" $
      run (string "test") "test" `shouldBe` Right "test"

  describe "failN failP failI" $ do
    it "failN" $ do
      let p = do
            void $ string "a"
            failN "error"
      (run p "ab" :: Either (ParseErrorBundle String Void) ())
        `shouldSatisfy` isLeft

    it "failP" $ do
      let p = do
            void $ string "a"
            failP "error"
      (run p "ab" :: Either (ParseErrorBundle String Void) ())
        `shouldSatisfy` isLeft

    it "failI" $ do
      let p = do
            void $ string "abc"
            failI 1 "error"
      (run p "abc" :: Either (ParseErrorBundle String Void) ())
        `shouldSatisfy` isLeft

  describe "additional coverage for fail messages" $ do
    it "failN sets the offset to the next token and includes the error message" $ do
      let p = do
            void $ string "abc"
            failN "failN error"
      let r = run p "abc"
      case r of
        Left bundle -> errorBundlePretty bundle `shouldContain` "failN error"
        Right _     -> expectationFailure "Expected failN to fail"

    it "failP sets the offset to the previous token and includes the error message" $ do
      let p = do
            void $ string "abc"
            failP "failP error"
      let r = run p "abc"
      case r of
        Left bundle -> errorBundlePretty bundle `shouldContain` "failP error"
        Right _     -> expectationFailure "Expected failP to fail"

    it "failI sets the offset to the given index and includes the error message" $ do
      let p = do
            void $ string "abcd"
            failI 2 "failI error"
      let r = run p "abcd"
      case r of
        Left bundle -> errorBundlePretty bundle `shouldContain` "failI error"
        Right _     -> expectationFailure "Expected failI to fail"

  describe "additional coverage for initialPos \"\"" $ do
    it "sets parser state to line 1 col 1 after (&>)" $ do
      let p1 = string "some" >> return "leftover"
      let p2 = fail "forced failure"
      let r = run (p1 &> p2) "someDATA"
      case r of
        Left bundle ->
          errorBundlePretty bundle `shouldContain` "1:1"
        Right _ ->
          expectationFailure "Expected an error at line 1 col 1"

operatorSpec :: Spec
operatorSpec = do
  describe "(&>)" $ do
    it "updates parser input" $ do
      let p1 = do
            _ <- string "old"
            getInput
      let p2 = string "new"
      run (p1 &> p2) "oldnew" `shouldBe` Right "new"
      (run (p1 &> p2) "oldx" :: Either (ParseErrorBundle String Void) String)
        `shouldSatisfy` isLeft

  describe "(!?)" $ do
    it "returns the element at index or Nothing" $ do
      let xs = [1 :: Int, 2, 3]
      xs !? 0 `shouldBe` Just 1
      xs !? 2 `shouldBe` Just 3
      xs !? 3 `shouldBe` Nothing
      xs !? (-1) `shouldBe` Nothing