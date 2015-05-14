module Denotational where
import qualified Data.Map as Map
import Syntax

data Approximation = 
 N Integer |
 A [(Approximation,Approximation)]
 deriving (Show, Eq)

--per ora omettiamo le coppie!!

filter2show :: Approximation -> Approximation
filter2show (N i) = N i
filter2show (A ax) = A 
 (filter 
  (filterBottoms) 
  ( map 
     (\(sx,dx) -> ((filter2show sx) ,(filter2show dx)))
     (take 5 ax)
  )
 )
 where
  filterBottoms :: (Approximation,Approximation) -> Bool
  filterBottoms ( _ , (A [] ) ) = False
  filterBottoms  _  = True


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

approx (IfThenElse e1 e2 e3) = \e -> (cond (approx e1 e) e2 e3) e
 where
  cond :: Approximation -> Expr -> Expr -> Environment -> Approximation
  cond (A _) _ _ = \e -> A []
  cond (N n) th el 
   | n == 0 = approx th
   | n /= 0 =  approx el
   | otherwise = \e -> A []

approx (LetIn name e1 e2) = \e -> ((approx e2) (insertEnv name ((approx e1) e) e))

approx (Rec name (Lam x t)) = \e -> 
 (approx (Lam x t) (insertEnv name (approx (Rec name (Lam x t)) e) e) )


--calcola approssimazioni di un rec!
muuu :: Expr -> Environment -> [ Approximation ]
muuu (Rec y (Lam x t)) e = 
 iterate ff (A [])
 where
  ff = \fi -> approx (Lam x t) (insertEnv y fi e) --fi è l'approssimazione precedente

--Anche un metodo per visualizzare le approssimazioni, scartando chi ha nel lato dx un bel bottom.


test1 = (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (Lit (LInt 3)))

test2 = Lam "x" (IfThenElse (Var "x") (Lit (LInt 666)) (Var "bottom") )

testrec = (App fact (Lit (LInt 10)) )

fact = (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))

testmuu = take 6 ( muuu fact emptyEnv )

--main = putStrLn $ show $ filter2show $ ( approx testrec emptyEnv )
main = putStrLn $ show $ map filter2show testmuu 
