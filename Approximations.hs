module Denotational where
import qualified Data.Map as Map
import Syntax

data Approximation = 
 N Integer |
 A [(Approximation,Approximation)]
 deriving (Show, Eq)

filter2show :: Approximation -> Approximation
filter2show (N i) = N i
filter2show (A ax) = A 
 ( map 
    (\(sx,dx) -> ((filter2show sx) ,(filter2show dx)))
    (take 5 ax)
 )


type Environment = Map.Map Name Approximation

emptyEnv :: Environment
emptyEnv = Map.empty

insertEnv :: Name -> Approximation -> Environment -> Environment
insertEnv = Map.insert

member :: Name -> Environment -> Bool
member = Map.member

modifyEnv :: Approximation -> Name -> Environment -> Environment
-- modifyEnv v x rho = rho[v/x]
modifyEnv v x rho = Map.insert x v rho

--proviamo a rappresentare alcune funzioni

approx :: Expr -> Environment -> Approximation
approx (Lit (LInt n)) = \e -> N n

approx (Var name) = \e -> if member name e then (e Map.! name) else A []
--TODO gestire il bottom come una lista vuota!! A []

approx (Sum e1 e2) = \e -> pluslift (approx e1 e) (approx e2 e)
 where
  pluslift (N a) (N b) = N (a+b)
  pluslift _ _ = A [] --caso in cui non posso fare la somma

approx (Sub e1 e2) = \e -> sublift (approx e1 e) (approx e2 e)
 where
  sublift (N a) (N b) = N (a-b)
  sublift _ _ = A [] --caso in cui non posso fare la sottrazione

approx (Mul e1 e2) = \e -> mullift (approx e1 e) (approx e2 e)
 where
  mullift (N a) (N b) = N (a*b)
  mullift _ _ = A [] --caso in cui non posso fare la mul

approx (Lam name expr) = \e -> A $ map ((funzioncina)e) [0..]
 where funzioncina = \e -> \n -> ( N n , ((approx expr) (insertEnv name (N n) e)) )

approx (App t1 t2) = \e -> searchAprx (approx t1 e) (approx t2 e)
 where
  searchAprx :: Approximation -> Approximation -> Approximation
  searchAprx (A ((a, y):ax)) x =
   if a == x then y else searchAprx (A ax) x
  searchAprx (A []) _ = A []
  searchAprx _ (A []) = A []

main = putStrLn $ show $ filter2show $ ( approx (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (Lit (LInt 3)))  emptyEnv )
