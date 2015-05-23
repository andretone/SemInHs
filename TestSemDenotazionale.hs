module Main where

import Denotational
import Syntax

main'' = putStrLn $ show $ denote testrec emptyEnv

main' = putStrLn $ show $ denote test'' envy'

test = (IfThenElse
 (Mul (Var "x") (Sub (Var "x") (Lit (LInt 4))))
 (First (Var "y"))
 (Second (Var "y"))
 )

test' = (Lam "v" (Sum (Var "v") (Lit (LInt 1))))

test'' = (LetIn "funzione"
          (Lam "e" (Lit(LInt 2)) )
          (App (Lam "v" (App (Var "v") ((Lit (LInt 1))))) (Var "funzione") )
         )

envy = insertEnv "x" (VI 1) (emptyEnv)
envy' = insertEnv "y" ( VP ((VI 5),(VI 6)) ) (envy)

testrec =
 (App
  (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))
  (Lit (LInt 10))
 )

--Dato il nostro programma, forniamo una serie di "punti" appartenenti a Tau da dare come argomento al programma,
-- osserviamo il comportamento
appPL :: Expr -> [Tau] -> [Maybe Tau]
appPL program args =
 case (denote program emptyEnv) of
  Just (VI i) -> take (length args) (repeat (Just (VI i)) )
  Just (VP p) -> take (length args) (repeat (Just (VP p)) )
  Just (VF fun) -> map fun args
  Nothing -> take (length args) (repeat Nothing)


fattoriale =
 (Rec "rec" (Lam "x" (IfThenElse (Var "x") (Lit (LInt 1)) (Mul(Var "x")(App (Var "rec")(Sub (Var "x")(Lit(LInt 1))))) )))

test_fattoriale =
 putStrLn $ show $ appPL fattoriale [(VI 4), (VI 5), (VI 6)]

pairFunction =
 (LetIn "c"
  (Lit (LInt 10))
  (Rec "rec" (Lam "pair" (IfThenElse (First (Var "pair"))
                                     (Var "pair")
                                     (App (Var "rec") (Lit $ LPair (Sub(First (Var "pair"))(Lit(LInt 1))) (Second (Var "pair"))))
                         )
             )
  )
 )

test_pair = putStrLn $ show $ appPL pairFunction [(VP (VI 1, VI 3))]

t_function =
 (Lam "f" (App (Var "f") (Lit (LInt 10))))

fa (VI x)= Just (VI 666)
fb (VI x)= Nothing
fc (VI x)= Just (VI $ x-1)

test_function = putStrLn $ show $ appPL t_function [(VF fa), (VF fb), (VF fc)]

main = main''
