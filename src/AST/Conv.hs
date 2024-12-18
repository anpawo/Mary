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

tokenToASTAssignVar :: String -> Token -> Token -> Either String AST
tokenToASTAssignVar varName IntT (IntLit num) = Right $ AstVar varName IntT (AstInt num)
tokenToASTAssignVar varName CharT (CharLit c) = Right $ AstVar varName CharT (AstChar c)
tokenToASTAssignVar varName FloatT (FloatLit float) = Right $ AstVar varName FloatT (AstFloat float)
tokenToASTAssignVar varName StrT (StringLit str) = Right $ AstVar varName StrT (AstStr str)
tokenToASTAssignVar varName BoolT (BoolLit bool) = Right $ AstVar varName BoolT (AstBool bool)
tokenToASTAssignVar _ _ _ = Left $ "Value must be the same type of variable"

tokenToAST :: Token -> Either String AST
tokenToAST (IntLit num) = Right $ AstInt num
tokenToAST (CharLit c) = Right $ AstChar c
tokenToAST (StringLit str) = Right $ AstStr str
tokenToAST (BoolLit bool) = Right $ AstBool bool
tokenToAST (FloatLit float) = Right $ AstFloat float
tokenToAST (SymbolId _varName) = Right $ AstWord _varName
tokenToAST (Line [ReturnKw, _value]) = case tokenToAST _value of
    Right res -> Right $ AstReturn res
    Left err -> Left err
tokenToAST (Line [_type, SymbolId _varName]) = Right $ AstVar _varName _type AstNull
tokenToAST (Line [_type, SymbolId _varName, Assign, _value]) = tokenToASTAssignVar _varName _type _value
--need to convert the body of the function
tokenToAST (FunctionSort {funcSortName = name, funcSortParam = params, funcSortType = _type, funcSortBody = body}) = case mapM tokenToAST params of
  Right args -> case mapM tokenToAST body of
    Right astBody -> Right $ AstFunction name _type args astBody
    Left err -> Left err
  Left err -> Left err
tokenToAST (StructureSort {structSortName = name, structSortBody = body}) = case mapM tokenToAST body of
  Right struct_body -> Right $ AstStructure  name struct_body
  Left err -> Left err
tokenToAST err = Left $ "Unrecognized Token " ++ show err
