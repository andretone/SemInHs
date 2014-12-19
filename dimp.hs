--interpreta il linguaggio IMP seguendo la sua semantica denotazionale 
--NB la semantica denotazionale dei comandi dovrebbe essere una funzione parziale, io non
--tengo conto del caso in cui il calcolo della semantica denotazionale vada in loop.

module Dimp where

import Control.Monad
import System.IO
import Control.Monad.State

import Control.Monad.Fix

type N = Int
type T = Bool
type Loc = String

data Aexp = Num N | X Loc | Sum Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
	
data Bexp = Bol T | Eq Aexp Aexp | Le Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp

data Com = Skip | Assign Loc Aexp | Seq Com Com | IfThenElse Bexp Com Com | While Bexp Com

--lo stato
type Sigma = Loc -> N

newState :: Sigma
newState = \x -> 0

stateInsert :: Sigma -> Loc -> N -> Sigma
stateInsert sigma loc n = (\x -> if  x == loc then n else sigma loc)

denotA :: Aexp -> (Sigma -> N)
denotA (Num n) = \sigma -> n
denotA (X str) = \sigma -> sigma str
denotA (Sum a0 a1) = \sigma -> ( ((denotA a0)sigma) + ((denotA a1)sigma) )
denotA (Sub a0 a1) = \sigma -> ( ((denotA a0)sigma) - ((denotA a1)sigma) )
denotA (Mul a0 a1) = \sigma -> ( ((denotA a0)sigma) * ((denotA a1)sigma) )

denotB :: Bexp -> (Sigma -> T)
denotB (Bol t) = \sigma -> t
denotB (Eq a1 a2) = \sigma -> ( ((denotA a1)sigma) == ((denotA a2)sigma) )
denotB (Le a1 a2) = \sigma -> ( ((denotA a1)sigma) <= ((denotA a2)sigma) )
denotB (Not b) = \sigma -> not((denotB b)sigma)
denotB (And b1 b2) = \sigma -> ( ((denotB b1)sigma) && ((denotB b2)sigma) )
denotB (Or b1 b2) = \sigma -> ( ((denotB b1)sigma) || ((denotB b2)sigma) )

denotC :: Com -> (Sigma -> Sigma)
denotC Skip = \sigma -> sigma
denotC (Assign str a) = \sigma -> (stateInsert sigma str ((denotA a)sigma))
denotC (Seq c0 c1) = (denotC c1) . (denotC c0)
denotC (IfThenElse b c0 c1) = 
	\sigma -> 
		case ((denotB b) sigma) of
			True -> (denotC c0) sigma
			False -> (denotC c1) sigma

denotC (While b c) = 
	fix 
		(\gamma sigma ->
			case (denotB b) sigma of
				True -> gamma ((denotC c)sigma)
				False -> sigma
		)

mioimp' =(Seq
                (Assign "var" (Num 10))
                (While
                        (Le (X "var") (Num 10))
                        (Assign "var" (Sum (X "var") (Num 1)))
                )
        )

mioimp'' = ( denotC (Seq (Assign "casa" (Num 10) ) (IfThenElse (Le (X "casa") (Num 30)) (Assign "lollo" (Sub (X "casa") (Num 20))) (Skip))) )

mioimp'''= --infinito, è pericoloso, ma è anche il comportamento che si vorrebbe.. no?
	(While
        	(Bol True)
                (Assign "var" (Sum (X "var") (Num 1)))
	)

main = 
	let f = 
		denotC mioimp''
	in
	putStrLn $ show (f newState "var")

--Esiste un metodo per confrontare le semantiche denotazionali?
