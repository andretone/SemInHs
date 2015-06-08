module Syntax where

 type Name = (String) 

 data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Expr
  | Sum Expr Expr | Sub Expr Expr | Mul Expr Expr
  | IfThenElse Expr Expr Expr
  | First Expr | Second Expr
  | LetIn Name Expr Expr | Fix Expr | Rec Name Expr
  deriving (Eq, Show)

 data Lit --literals 
  = LInt Integer
  | LPair Expr Expr
  deriving (Show, Eq ) --Ord
