module Macra.VM where

data Value = Int Int
           | Char Char
           | List [Value]

data Identifier = Sym String | Nil

data Expr = FrameExpr Expr
          | ConstExpr Value Expr
          | ArgExpr Expr
          | CloseExpr Identifier Expr Expr
          | ApplyExpr
          | ReferExpr Identifier Expr
          | ReturnExpr
          | TestExpr Expr Expr Expr
          | DefineExpr Identifier Expr
          | HaltExpr
          | PrintExpr Expr
