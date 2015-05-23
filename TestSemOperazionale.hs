module Main where

import LamUntiped
import Syntax
--TESTS
anothertest =(Lam "q" (App (Lam "a" (Lam "b" (App (Var "b") (Var "a")))) (App (Lam "z" (Lam "c" (Var "c"))) (Lit (LInt 1)) ) ) )

test = (App (Lam "y" (App (Var "s") (Var "y"))) (Lit (LInt 1)) )

controversial = (App (Lam "y" (Lam "x" (App (Var "y") (Var "x")) )) (Var "x"))

tt = (Lam "x" (Lam "y" (App (Lam "x" (App (Var "x")(App (Var "x")(Var "y")))) (Lam "z" (App (Var "z")(Var "x")))) ))

verytest =
        (App
                (Lam
                        "x"
                        (App
                          (Lam
                            "y"
                            (Lam "x" (App (Var "y") (Var "x" ))) )
                          (Var "x")
                        )
                )
                (Lam "e" (Syntax.Sum (Var "e") (Lit(LInt 1))))
        )

verydifficult =
                (App
                        (App
                                (Lam
                                        "x"
                                        (App
                                                (Lam
                                                        "y"
                                                        (Lam "x" (App (Var "y") (Var "x" ))) )
                                                (Var "x")
                                        )
                                )
                                (Lam "e" (Syntax.Sum (Var "e") (Lit(LInt 1))))
                        )
                        (Lit (LInt 1))
                )

t0 = ( LetIn
         "x"
         (Lam "y" (Var "y"))
         (App (Var "x") (Lit (LInt 1)))
     )

t1 =    (Syntax.Second (IfThenElse (Sub (Lit (LInt 2)) (App (Lam "x" (Syntax.Sum (Lit (LInt 2)) (Var "x") ) ) (Lit (LInt 1)) ))
                (Lit (LInt 1))
                (Lit (LPair (Lit (LInt 11)) (Lit (LInt 12)) ))
        ))

t2 = ( LetIn
         "x"
         (Lam "y" (Syntax.Sum (Lit (LInt 2)) (Var "y")))
         (App (Var "x") (Lit (LInt 1)))
     )

t3 = ( LetIn
         "x"
         (Syntax.Sum (Var "x") (Var "x"))
         (Var "x")
     )

fact' = (Lam "rec" (Lam "n" (IfThenElse (Var "n") (Lit (LInt 1)) (Mul (Var "n") (App (Var "rec") (Sub (Var "n") (Lit(LInt 1))))))))

testrec = App (Fix $ fact') (Lit (LInt 10))

provaRec = (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))

main =  do
 putStrLn $ show provaRec
 putStrLn $ show $ fst $ runEval (App (provaRec ) (Lit (LInt 9)))

main' = putStrLn $ show $ v2e $ fst $ runEval anothertest
