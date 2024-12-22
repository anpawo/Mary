{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}
{-# LANGUAGE RecordWildCards   #-}

module Ast.Error (errNameTaken, errImpossibleCase, prettyPrintError, errExpectedType) where

import Text.Megaparsec.Error (ParseErrorBundle(..), ParseError(..), ErrorFancy(..))
import Parser.Token
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.Internal (elemAt)

errNameTaken :: String -> String
errNameTaken name = "name '" ++ (purple name) ++ "' already taken."

errExpectedType :: Bool -> String
errExpectedType voidOk
    | voidOk = "expected a type (including void)."
    | otherwise = "expected a type (excluding void)."

errImpossibleCase :: String
errImpossibleCase = "Impossible case."

prettyPrintError :: [MyToken] -> ParseErrorBundle [MyToken] Void -> String
prettyPrintError tokens (ParseErrorBundle {bundleErrors = errors, bundlePosState = _}) =
    case errors of
        (FancyError pos fancySet :| _) ->
            case (0 `elemAt` fancySet) of
                (ErrorFail err) -> " |\n | " ++ tokCons ++ (red tokErr) ++ tokLeft ++ "\n |" ++ (red pointer) ++ "\n" ++ (red "error") ++ ": " ++ err
                x -> "This error should be transformed into a custom one:\n" ++ show x
            where
                tokCons = unwords $ show <$> tc
                -- tokCons = unwords $ elAt (pos - 3) ++ elAt (pos - 2) ++ elAt (pos - 1)
                    where
                        tc = take (pos - 1) tokens
                --         elAt i = case tc !? i of
                --             Nothing -> []
                --             Just x -> [show x]
                --         (!?) :: [a] -> Int -> Maybe a
                --         xs !? n
                --             | n < 0     = Nothing
                --             | otherwise = foldr (\x r k -> case k of
                --                                             0 -> Just x
                --                                             _ -> r (k-1)) (const Nothing) xs n
                tokErr  = " " ++ show (tokens !! (pos - 1)) ++ " "
                tokLeft = let tc = take 3 (drop pos tokens) in unwords $ show <$> tc
                pointer = replicate (length tokCons + 2) ' ' ++ (replicate (length tokErr - 2) '^')
        x -> "This error should be transformed into a custom one" ++ show x

red :: String -> String
red s = "\ESC[91m" ++ s ++ reset

purple :: String -> String
purple s = "\ESC[95m" ++ s ++ reset

reset :: String
reset = "\ESC[0m"
