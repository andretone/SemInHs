{-# LANGUAGE GADTs #-}

module Denotational where

import Syntax

import qualified Data.Map as Map

type N = Integer  

data Tau where
 VI :: N -> Tau 
 VP :: (Tau , Tau) -> Tau
 VF :: (Tau -> Maybe Tau) -> Tau

instance Show Tau where
  show (VI x) = show x
  show (VF a) = "<< Lambda  >>"
  show (VP (a, b)) = "(" ++ (show a) ++ " , " ++ (show b) ++ ")"

--environment  rho : Var -> Union(Vtau | tau a type) 
-- Ambiente serve per fornire un valore alle variabili libere
type Environment = Map.Map Name Tau
-- rispetterà x : tau => tho(x) in Vtau

emptyEnv :: Environment
emptyEnv = Map.empty

insertEnv :: Name -> Tau -> Environment -> Environment
insertEnv = Map.insert

member :: Name -> Environment -> Bool
member = Map.member 

modifyEnv :: Tau -> Name -> Environment -> Environment
-- modifyEnv v x rho = rho[v/x]
modifyEnv v x rho = Map.insert x v rho

denote :: Expr -> Environment -> Maybe Tau

denote (Var x) = 
 \e -> if member x e then Just (e Map.! x) else Nothing

denote (Lit (LInt n)) = \e -> Just (VI n)

denote (Sum t1 t2) = \e -> sumLifted (denote t1 e) (denote t2 e)
 where
  sumLifted (Just (VI a)) (Just (VI b)) = Just $ VI (a + b)
  sumLifted (Just (VI _)) Nothing = Nothing
  sumLifted Nothing (Just (VI _)) = Nothing
  sumlifted _ _ = error "invalid sum"

denote (Sub t1 t2) = \e -> subLifted (denote t1 e) (denote t2 e)
 where
  subLifted (Just (VI a)) (Just (VI b)) = Just $ VI (a - b)
  subLifted (Just (VI _)) Nothing = Nothing
  subLifted Nothing (Just (VI _)) = Nothing
  sublifted _ _ = error "invalid sub"

denote (Mul t1 t2) = \e -> mulLifted (denote t1 e) (denote t2 e)
 where
  mulLifted (Just (VI a)) (Just (VI b)) = Just $ VI (a * b)
  mulLifted (Just (VI _)) Nothing = Nothing
  mulLifted Nothing (Just(VI _)) = Nothing
  mulLifted _ _ = error "invalid mul"

denote (IfThenElse t0 t1 t2 ) = \e -> cond (denote t0 e) (denote t1 e) (denote t2 e)
 where
  cond :: Maybe Tau -> Maybe Tau -> Maybe Tau -> Maybe Tau
  cond z0 z1 z2 = case z0 of
   Just (VI 0) -> z1
   Just (VI _) -> z2
   _ -> error "invalid condition IfThEl"

denote (Lit (LPair t1 t2)) = \e -> liftedPair (denote t1 e) (denote t2 e)
 where
  liftedPair (Just a) (Just b) = Just (VP (a , b))
  liftedPair _ Nothing = Nothing
  liftedPair Nothing _ = Nothing

denote (First t) = \e -> liftedProjection1 (denote t e)
 where
  liftedProjection1 (Just (VP (a , b))) = Just a
  liftedProjection1 Nothing = Nothing

denote (Second t) = \e -> liftedProjection2 (denote t e)
 where
  liftedProjection2 (Just (VP (a, b))) = Just b
  liftedProjection2 Nothing = Nothing

denote (Lam x t) = \e -> 
 Just ( VF (\v -> denote t (modifyEnv v x e) )) --v deve essere un elem di Vt1 e ( \x.t : t1 -> t2)

denote (App t1 t2) = \e -> 
 let fi = denote t1 e
     Just v  = denote t2 e
 in case fi of
  Just (VF fun) -> fun v
  _ -> error "bad application"

denote (LetIn x t1 t2) = \e ->
 let Just v = denote t1 e --verificare correttezza di ciò, siamo eager, magari giusto così, 
--altrimenti con case? forzerebbe l'esecuzione, potrebbe anche starci
 in  (denote t2 (modifyEnv v x e) )

denote (Rec y (Lam x t)) = \e ->
 Just 
  muuu $ fi (\v -> (denote t (modifyEnv v x (modifyEnv fi y e))))

denote _ = error "not implemented"

main = putStrLn $ show $ denote test'' envy'

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
