module Eager where

import Control.Monad
import System.IO
import Control.Monad.State

import Data.Set -- per le variabili free

type N = Int
type Loc = String

data Tau = N | PairType Tau Tau | FunType Tau Tau 
	deriving (Ord,Eq)

type Var = (Loc, Tau)


data Term = 	X Var | --variabili x : tau
		Num N | Sum Term Term | Sub Term Term | Mul Term Term | IfThenElse Term Term Term |
		Pair Term Term | First Term | Second Term |
		Function Var Term | Apply Term Term | LetIn Var Term Term |
		Rec Var Var Term 

--typing rules
tipo :: Term -> Tau

--variabile
tipo (X (loc, tau)) = tau

--operazioni
tipo (Num n) = N

tipo (Sum t1 t2) = if (((tipo t1) == N) && ((tipo t2) == N)) then N else error "type exception"
tipo (Sub t1 t2) = if (((tipo t1) == N) && ((tipo t2) == N)) then N else error "type exception"
tipo (Mul t1 t2) = if (((tipo t1) == N) && ((tipo t2) == N)) then N else error "type exception"

tipo (Pair t1 t2) = PairType (tipo t1) (tipo t2)

tipo (Function var term) = FunType (tipo (X var)) (tipo term)

tipo (Apply t1 t2) = 
	let (FunType tau1 tau2) = (tipo t1) in
		if (tipo t2) == tau2 then N else error "type exception"

tipo (LetIn var t1 t2) = tipo t2

tipo (Rec y x t) = 
	let tau = (tipo (X y)) in 
		if tau == (tipo (Function x t)) then tau else error "type exception"


--Freee variables
fv :: Term -> Set Var
fv (Num n) = empty
fv (X var) = singleton var
fv (Sum t1 t2) = union (fv t1) (fv t2)
fv (Sub t1 t2) = union (fv t1) (fv t2)
fv (Mul t1 t2) = union (fv t1) (fv t2)
fv (IfThenElse t0 t1 t2) = union (union (fv t0) (fv t1)) (fv t2)
fv (Pair t1 t2) = union (fv t1) (fv t2)
fv (First t) = fv t
fv (Second t) = fv t
fv (Function x t) = difference (fv t) (singleton x)
fv (Apply t1 t2) = union (fv t1) (fv t2)
fv (LetIn var t1 t2) = (union (fv t1) (difference (fv t2) (singleton var)))
fv (Rec y x t) = difference (fv (Function x t)) (singleton y)

isClosed :: Term -> Bool
isClosed t = Data.Set.null (fv t)

--canonical forms
isCe :: Term -> Bool
isCe (Num n) = True -- n appartiene a Ce Int
isCe (Pair t1 t2) = (isCe t1) && (isCe t2)
isCe lamb@(Function x t) = (isClosed lamb) -- ((tipo lamb) == (FunType _ _)) && (isClosed lamb) sottointeso che sia del tipo FunType?!

--evaluation!
--le forme canniche valutano loro stesse
evaluate :: Term -> Term
evaluate n@(Num a) = if isCe n then n else error "Num is not ce"
evaluate pair@(Pair t1 t2) = if (isCe pair) then pair else error "Pair is not ce"
evaluate lamb@(Function x t) = if isCe lamb then lamb else error "Lamb is not ce"

evaluate (Sum t1 t2) = 
	let
		Num n1 = evaluate t1
		Num n2 = evaluate t2
	in
		Num (n1+n2)

evaluate (Sub t1 t2) = 
        let
                Num n1 = evaluate t1
                Num n2 = evaluate t2
        in
                Num (n1-n2)

evaluate (Mul t1 t2) = 
        let
                Num n1 = evaluate t1
                Num n2 = evaluate t2
        in
                Num (n1*n2)

evaluate (IfThenElse t0 t1 t2) = 
	case (evaluate t0) of
	 (Num 0) -> evaluate t1
	 _ ->  evaluate t2 

evaluate (Pair t1 t2) = let
                c1 = evaluate t1
                c2 = evaluate t2
        in
                if isCe c1 && isCe c2 then (Pair c1 c2) else error "not canonical pair form"

evaluate (First  t) = let
                Pair c1 _  = evaluate t
        in
                c1

evaluate (Second  t) = let
                Pair _ c2  = evaluate t
        in
                c2



main =
	case 
		evaluate 
			(IfThenElse (Sum (Num (-1)) (Num 1)) (Pair (Pair (Num 1)(Num 2)) (Num 0)) (Num (-100)) )
	of
	(Num n) -> putStrLn $ show n
	(Pair a b ) -> putStrLn "paio"
	(Function x t) -> putStrLn "funzione"
