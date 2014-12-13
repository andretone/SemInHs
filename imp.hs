{-# LANGUAGE GADTs #-}
module Imp where

import Control.Monad
import System.IO
import Control.Monad.State

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

--eval for Aexp
evalA :: Aexp -> State Sigma N --lavora sullo stato, torna la valutazione
evalA (Num n) = state (\s -> (n,s))
evalA (X str) = state (\sigma -> (sigma str , sigma))
evalA (Sum aexp1 aexp2) = state (\sigma -> ( ( (evalState (evalA aexp1) sigma) +  (evalState (evalA aexp2) sigma) ) , sigma)) 
evalA (Sub aexp1 aexp2) = state (\sigma -> ( ( (evalState (evalA aexp1) sigma) -  (evalState (evalA aexp2) sigma) ) , sigma))
evalA (Mul aexp1 aexp2) = state (\sigma -> ( ( (evalState (evalA aexp1) sigma) *  (evalState (evalA aexp2) sigma) ) , sigma))

-- eval for Bexp
evalB :: Bexp -> State Sigma T
evalB (Bol t) = state (\s -> (t,s))
evalB (Eq aexp1 aexp2) = state ( \sigma -> ( ( (evalState (evalA aexp1) sigma) == (evalState (evalA aexp2) sigma) ) , sigma))
evalB (Le aexp1 aexp2) = state ( \sigma -> ( ( (evalState (evalA aexp1) sigma) <= (evalState (evalA aexp2) sigma) ) , sigma))
evalB (Not bexp) = state ( \sigma -> ( not (evalState (evalB bexp) sigma), sigma))
evalB (And bexp1 bexp2) = state ( \sigma -> ( ( (evalState (evalB bexp1) sigma) && (evalState (evalB bexp2) sigma) ) , sigma))
evalB (Or bexp1 bexp2) = state ( \sigma -> ( ( (evalState (evalB bexp1) sigma) || (evalState (evalB bexp2) sigma) ) , sigma))

--eval for Com
evalC :: Com -> State Sigma Sigma 
evalC Skip = state ( \s -> (s,s))
evalC (Assign loc aexp) = state (\s ->
					let s' = stateInsert s loc (evalState (evalA aexp) s) in
						(s', s')
			)
evalC (Seq c1 c2) = 
	state
		(\s ->
			runState (evalC c2) (evalState (evalC c1) s)
		)

evalC (IfThenElse bexp c1 c2) = 
	state( \s ->
		if (evalState (evalB bexp) s) == True 
		then runState (evalC c1) s
		else runState (evalC c2) s
	)
evalC (While bexp com) =
        state( \s ->
                if (evalState (evalB bexp) s) == True
                then runState (evalC (Seq com (While bexp com))) s
                else runState (evalC Skip) s
        )


mioimp =(Seq
		(Assign "var" (Num 10))
		(While 
			(Not (Le (X "var") (Num 0)))
			(Assign "var" (Sub (X "var") (Num 1)))
		)
	)
mioimp' =(Seq
                (Assign "var" (Num 10))
		(While 
			(Le (X "var") (Num 10))
                	(Assign "var" (Sum (X "var") (Num 1)))
        	)
	)

run :: Com -> Sigma
run code = evalState (evalC code) newState

runA :: Aexp -> N
runA a = evalState (evalA a) newState

runB :: Bexp -> T
runB b = evalState (evalB b) newState

main = 
	do
		putStrLn $ show ((evalState (evalC mioimp) (newState)) "var")
		putStrLn $ show $ runA (Sum (Mul (X "var") (Num 1)) (Sub (Num 2) (Num 80)))
		putStrLn $ show $ runB (Bol True)
