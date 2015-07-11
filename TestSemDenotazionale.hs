module Main where

import Denotational
import Syntax hiding (Lam', T)

main'' = putStrLn $ show $ denote testrec emptyEnv

main' = putStrLn $ show $ denote test'' envy'

test = (IfThenElse
 (Mul (Var "x") (Sub (Var "x") (LInt 4)))
 (First (Var "y"))
 (Second (Var "y"))
 )

test' = (Lam "v" (Sum (Var "v") (LInt 1)))

test'' = (LetIn "funzione"
          (Lam "e" (LInt 2) )
          (App (Lam "v" (App (Var "v") ((LInt 1)))) (Var "funzione") )
         )

envy = insertEnv "x" (VI 1) (emptyEnv)
envy' = insertEnv "y" ( VP ((VI 5),(VI 6)) ) (envy)

testrec =
 (App
  (Rec "rec" (Lam "x" (IfThenElse (Var "x") (LInt 1) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(LInt 1)))) )))
  (LInt 10)
 )

fattoriale =
 (Rec "rec" 
  (Lam "x" 
   (IfThenElse
    (App isPos (Var "x"))
    (IfThenElse (Var "x") (LInt 1) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(LInt 1)))) )
    (Bottom) --se negativo
   )
  )
 )

test_fattoriale =
 putStrLn $ show $ appPL fattoriale [(VI 4), (VI 5), (VI 6)]

pairFunction =
 (LetIn "c"
  (LInt 10)
  (Rec "rec" (Lam "pair" (IfThenElse (First (Var "pair"))
                                     (Var "pair")
                                     (App (Var "rec") (LPair (Sub(First (Var "pair"))(LInt 1)) (Second (Var "pair")))))
             )
  )
 )

test_pair = putStrLn $ show $ appPL pairFunction [(VP (VI 1, VI 3))]

t_function =
 (Lam "f" (App (Var "f") (LInt 10)))

fa (VI x)= Just (VI 666)
fb (VI x)= Nothing
fc (VI x)= Just (VI $ x-1)

test_function = putStrLn $ show $ appPL t_function [(VF fa), (VF fb), (VF fc)]


--test per appPL
programma = (Lam "fun" (App (Var "fun") (LInt 5)) )
--parametri tau da fornire ad appPL
funA = VF (\y -> case y of
               VI n -> Just $ VI (n + 3)
               VP _ -> error "VP"
               VF _ -> error "VF"
       )
funB = VF (\y -> case y of
               VI n -> Just $ VP (( VI $ n + 3), (VI 4))
               VP _ -> error "VP"
               VF _ -> error "VF"
       )


tapp = appPL programma [funA , funB]

tapp' = appPL fattoriale [VI 1, VI 2, VI 3]

main = main''

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
                                      
