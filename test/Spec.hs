{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
-}


import TokenizerSpec (spec)

import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Tokenizer (Parser)" TokenizerSpec.spec
