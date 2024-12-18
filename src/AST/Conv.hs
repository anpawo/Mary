{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Conv
-}

module AST.Conv
  (
    tokenToAST
  )
where

import AST.Data
import Parser.Token

tokenToAST :: Token -> Either String AST
tokenToAST (IntLit num) = Right $ AstInt num
tokenToAST (CharLit c) = Right $ AstChar c
tokenToAST (StringLit str) = Right $ AstStr str
tokenToAST (BoolLit bool) = Right $ AstBool bool
tokenToAST (FloatLit float) = Right $ AstFloat float
tokenToAST (Line [_type, SymbolId _varName]) = Right $ AstVar _varName _type AstNull
--need to convert the body of the function
tokenToAST (FunctionSort {funcSortName = name, funcSortParam = params, funcSortType = _type, funcSortBody = body}) = case mapM tokenToAST params of
  Right args -> Right $ AstFunction name _type args
  Left err -> Left err
tokenToAST (StructureSort {structSortName = name, structSortBody = body}) = case mapM tokenToAST body of
  Right struct_body -> Right $ AstStructure  name struct_body
  Left err -> Left err
tokenToAST err = Left $ "Unrecognized Token " ++ show err
