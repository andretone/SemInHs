module Main where

import Approximations
import Syntax

test1 = (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (Lit (LInt 3)))

test2 = Lam "x" (IfThenElse (Var "x") (Lit (LInt 666)) (Var "bottom") )

testrec = (App fact (Lit (LInt 10)) )

fact = (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))

testmuu = take 6 ( muuu fact emptyEnv )

main = do
 putStrLn $ show $ map filter2show testmuu
 putStrLn $ show $ filter2show $ ( approx testrec emptyEnv )
