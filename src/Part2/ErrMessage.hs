{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrMessage
-}

module Part2.ErrMessage (errType, errArgNb, errJump) where

errType :: String -> String -> String
errType i t = "Error: Invalid type for instruction " ++ i ++ " expected " ++ t

errArgNb :: String -> Int -> String
errArgNb i n = "Error: Invalid number of arguments for instruction " ++ i ++ " expected " ++ show n ++ " arguments"

errJump :: String
errJump = "Error: Invalid jump, stack is empty"
