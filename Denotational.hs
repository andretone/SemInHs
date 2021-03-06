--SEMANTICA DENOTAZIONALE--

{-# LANGUAGE GADTs #-}

module Denotational
( Tau (..)
, denote
, emptyEnv
, insertEnv
, appPL
) where

import Syntax hiding (Lam', T)

import qualified Data.Map as Map
import Data.Maybe

type N = Integer  

{-
un termine t denota un elemento di  Maybe(Tau)
Tau rappresenta il datatype dei CPO

il datatype utilizza i tipi generalizzati algebrici
di haskell (GADTs, vedi pragma iniziale)
-}
data Tau where
 VI :: N -> Tau 
 VP :: (Tau , Tau) -> Tau
 VF :: (Tau -> Maybe Tau) -> Tau

instance Show Tau where
  show (VI x) = show x
  show (VF a) = "<< Lambda  >>"
  show (VP (a, b)) = "(" ++ (show a) ++ " , " ++ (show b) ++ ")"

{-
environment  rho : Var -> Union(Vtau | tau a type) 
Ambiente serve per fornire un valore alle variabili libere

Map funziona come un dizionario
-}
type Environment = Map.Map Name Tau

emptyEnv :: Environment
emptyEnv = Map.empty

insertEnv :: Name -> Tau -> Environment -> Environment
insertEnv = Map.insert

member :: Name -> Environment -> Bool
member = Map.member 

-- modifyEnv v x rho = rho[v/x]
modifyEnv :: Tau -> Name -> Environment -> Environment
modifyEnv v x rho = Map.insert x v rho

{-
Dato un termine torna la sua semantica denotazionale
Nothing indica l'elemento bottom

denote è definita seguendo le regole della semantica 
denotazionale del libro di Winskel
-}

denotationalSemOf ::Expr -> Maybe Tau
denotationalSemOf t = denote t emptyEnv

denote :: Expr -> Environment -> Maybe Tau
denote Bottom = \e -> Nothing
denote (Var x) = 
 \e -> if member x e then Just (e Map.! x) else Nothing

denote (LInt n) = \e -> Just (VI n)

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

denote (IfThenElse t0 t1 t2 ) = 
 \e -> cond (denote t0 e) (denote t1 e) (denote t2 e)
  where
   cond :: Maybe Tau -> Maybe Tau -> Maybe Tau -> Maybe Tau
   cond z0 z1 z2 = case z0 of
    Just (VI 0) -> z1
    Just (VI _) -> z2
    Nothing -> Nothing
    _ -> error "invalid condition IfThEl"

denote (LPair t1 t2) = \e -> liftedPair (denote t1 e) (denote t2 e)
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
 Just ( VF (\v -> denote t (modifyEnv v x e) )) 
 --v deve essere un elem di Vt1 e ( \x.t : t1 -> t2)

denote (App t1 t2) = \e -> 
 let fi = denote t1 e
     dv  = denote t2 e
 in case fi of
  Just (VF fun) -> 
   case dv of
    Just v -> fun v
    Nothing -> Nothing
  Nothing -> Nothing
  _ -> error "bad application"

denote (LetIn x t1 t2) = \e ->
 let dv = denote t1 e 
 in
  case dv of
   Just v -> (denote t2 (modifyEnv v x e) )
   Nothing -> Nothing

denote (Rec y (Lam x t)) = 
 \e ->
  Just $ VF
       ((\f -> \v -> 
         (denote t (modifyEnv f y (modifyEnv v x e)))) 
	 (fromJust $ denote (Rec y (Lam x t)) e))

denote _ = error "not implemented"

{-
Dato il nostro programma, forniamo una serie di "punti" 
appartenenti a Tau da dare come argomento al programma,
osserviamo il comportamento

appPL prende 
un termine del linguaggio (programma su cui si vuole testare il comportamento)
una lista di Tau,

torna:
Una lista di risultati dell'applicazione dei valori della lista
su il nostro programma.

Ovviamente se la semantica denotazionale del programma
è nell'insieme dei numeri interi o delle coppie non ha
senso fare l'applicazione (caso Just (VI i) e Just (VP p))

ES:
Prelude Syntax> :l Denotational
[2 of 2] Compiling Denotational     ( Denotational.hs, interpreted )
Ok, modules loaded: Denotational, Syntax.
*Denotational> :m + Syntax 
*Denotational Syntax> let prog = (Lam "x" (Sum (LInt 4) (Var "x")))
*Denotational Syntax> denote prog emptyEnv 
Just << Lambda  >>
*Denotational Syntax> appPL prog [VI 3, VI 4, VI 5]
[Just 7,Just 8,Just 9]

ES:
*Denotational Syntax> let prog = (Lam "x" (App (Var "x") (LInt 5)))
*Denotational Syntax> appPL prog [(VF $ \a -> Just $ VI (1) ), (VF $ \a -> Nothing)]
[Just 1,Nothing]

ES:
> appPL fattoriale [VI 1, VI 2, VI 3]
[Just 1,Just 2,Just 6]


ES. stravagante (presente anche in TestSemDenotazionale.hs):
programma = (Lam "fun" (App (Var "fun") (LInt 5)) )
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
> tapp 
[Just 8,Just (8 , 4)]

come si può vedere le funzioni in Tau descritte da noi possono tranquillamente
fare cose arbitrarie, basta che restino in Tau

-}
appPL :: Expr -> [Tau] -> [Maybe Tau]
appPL program args =
 case (denote program emptyEnv) of
  Just (VI i) -> take (length args) (repeat (Just (VI i)) )
  Just (VP p) -> take (length args) (repeat (Just (VP p)) )
  Just (VF fun) -> map fun args
  Nothing -> take (length args) (repeat Nothing)
