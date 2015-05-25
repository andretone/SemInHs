module Main where

import Approximations
import Syntax


--calcola approssimazioni di un rec!
muuu :: Expr -> Environment -> [ Approximation ]
muuu (Rec y (Lam x t)) e =
 iterate ff (A [])
 where
  ff = \fi -> approx (Lam x t) (insertEnv y fi e) --fi Ã¨ l'approssimazione precedente

--Anche un metodo per visualizzare le approssimazioni, scartando chi ha nel lato dx un bel bottom.


test1 = (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (Lit (LInt 3)))

test2 = Lam "x" (IfThenElse (Var "x") (Lit (LInt 666)) (Var "bottom") )

testrec = (App fact (Lit (LInt 10)) )

fact = (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))

testmuu = take 6 ( muuu fact emptyEnv )

--main = putStrLn $ show $ filter2show $ ( approx testrec emptyEnv )
main = putStrLn $ show $ map filter2show testmuu
