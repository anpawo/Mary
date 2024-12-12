{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}

module TokenizerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, Expectation)

import Data.Either (isLeft)

import Tokenizer

tsucc :: (Show a, Eq a) => Parser a -> String -> a -> Expectation
tsucc parser input expected = runParser parser "" input `shouldBe` (Right expected)

tfail :: (Show a) => Parser a -> String -> Expectation
tfail parser input = runParser parser "" input `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "edge case" $ do
    it "no comment" $
      tsucc rmComments "x = 5\ny = 6" "x = 5\ny = 6"
    it "unfinished string" $
      tfail rmComments "x = 5\"y = 6"

  describe "remove single line comment" $ do
    it "terminated by \\n" $
      tsucc rmComments "x = 5// variable x\ny = 6" "x = 5\ny = 6"
    it "terminated by eof" $
      tsucc rmComments "x = 5// variable x" "x = 5"
    it "with string" $
      tsucc rmComments "x = \"lol\"// variable x" "x = \"lol\""

  describe "remove multi line comment" $ do
    it "on single line" $
      tsucc rmComments "x = 5/* variable x*/\ny = 6" "x = 5\ny = 6"
    it "on two lines" $
      tsucc rmComments "x = 5/*\ncomment*/variable x" "x = 5variable x"
    it "missing closing part" $
      tfail rmComments "x = 5/*\ncommentvariable x"
    it "with string" $
      tsucc rmComments "x = \"lol\"/* variable x*/\ny = 6" "x = \"lol\"\ny = 6"
