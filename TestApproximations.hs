module Main where

import Approximations
import Syntax

test1 = (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (LInt 3))

test2 = Lam "x" (IfThenElse (Var "x") (LInt 666) (Var "bottom") )

testrec = (App fact (LInt 10) )

fact = (Rec "rec" (Lam "x" (IfThenElse (Var "x") (LInt 1) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(LInt 1)))) )))

testmuu = take 6 ( muuu fact emptyEnv )

main = do
 putStrLn $ show $ map (filter2show 8) testmuu
 putStrLn $ show $ filter2show 8 $ ( denote' testrec emptyEnv )
