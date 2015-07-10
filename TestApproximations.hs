module Main where

import Approximations
import Syntax

test1 = (App (Lam "y" (Lam "x" (Sum (Var "x") (Var "y")))) (LInt 3))

test2 = Lam "x" (IfThenElse (Var "x") (LInt 666) (Var "bottom") )

testrec = (App fact (LInt 10) )

fact =
 (Rec "rec"
  (Lam "x"
   (IfThenElse
    (App isPos (Var "x"))
    (IfThenElse (Var "x") (LInt 1) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(LInt 1)))) )
    (IfThenElse menox (LInt 1) (Mul menox (App (Var "rec")(Sub menox (LInt 1)))) )
   )
  )
 ) 

menox = (Mul (Var "x") (LInt (-1)))

testmuu = take 6 ( muuu fact emptyEnv )

main = do
 putStrLn $ show $ map (filter2show 8) testmuu
 putStrLn $ show $ filter2show 8 $ ( denote' testrec emptyEnv )


{- determinare se un intero e' negativo o positivo, tramite sottrazioni successive -}
{- negativo, torno -1, altrimenti torno 0 -}

isPos =
 (Lam "n"
 (App
 (App
 (Rec "rec"
  (Lam "x"
   (Lam "y"
    (IfThenElse (Var "x") (LInt (0))
                         (IfThenElse (Var "y") (LInt (-1))
                                               (App (App (Var "rec") (decx)) (incy))
                         )
    )
   )
  )
 )
 (Var "n")
 )
 (Var "n")
 )
 )

 where
  decx = (Sub (Var "x") (LInt 1))
  incy = (Sum (Var "y") (LInt 1))
