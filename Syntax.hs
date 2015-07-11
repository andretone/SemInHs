module Syntax where

 data T = Num
        | Couple T T
        | Function
  deriving (Eq)

 type Name = (String) 

 data Expr
  = Bottom
  | Var Name
  | LInt Integer
  | LPair Expr Expr
  | App Expr Expr
  | Lam Name Expr
  | Lam' Name T Expr
  | Sum Expr Expr | Sub Expr Expr | Mul Expr Expr
  | IfThenElse Expr Expr Expr
  | First Expr | Second Expr
  | LetIn Name Expr Expr | Fix Expr | Rec Name Expr
  deriving (Eq)

 instance Show Expr where
   show Bottom = "_|_"
   show (Var n) = show n
   show (LInt i) = show i
   show (LPair e1 e2) = show (e1, e2)
   show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
   show (Lam name expr) = "(" ++ " \\ " ++ show name ++ "." ++ show expr ++ ")"
   show (Lam' name _  expr) = "(" ++ " \\ " ++ show name ++ "." ++ show expr ++ ")"
   show (Sum e1 e2) = show e1 ++ "+" ++ show e2
   show (Sub e1 e2) = show e1 ++ "-" ++ show e2
   show (Mul e1 e2) = show e1 ++ "*" ++ show e2
   show (IfThenElse e0 e1 e2) = "if" ++ show e0 ++ " then " ++ show e1 ++ " else " ++ show e2
   show (First e) = "fst(" ++ show e ++ ")"
   show (Second e) = "snd(" ++ show e ++ ")"
   show (LetIn n e1 e2) = "let" ++ show n ++ "=" ++ show e1 ++  "in" ++ show e2
   show (Fix e) = "Fix" ++ show e
   show (Rec name e) = "Rec" ++ show name ++ "." ++ show e
