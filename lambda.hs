module Lambda where

import Control.Monad
import System.IO
import Control.Monad.State

import Data.Set -- per le variabili free

import Control.Monad.Trans.Error

data Term_up = 		--inferable terms
	Ann Term_down Type 	--termini annotati
	| Bound Int 		--variabili bound
	| Free Name 		--variabili free
	| Term_up :@: Term_down	--operatore :@: denota l'applicazione
	deriving (Show, Eq)

--When passing a binder in an algorithm, we have to convert a bound variable 
--into a free variable temporarily, and use Local for that

data Term_down = 	--checkable term
	Inf Term_up		--contiene i termini inferibili
	| Lam Term_down		--lambda astrazioni (no variabile, usiamo gli indici di bruijn
	deriving (Show, Eq)

data Name =
	Global String
	| Local Int
	| Quote Int
	deriving (Show, Eq)

data Type =
	TFree Name
	| Fun Type Type
	deriving (Show, Eq)

data Value =
	VLam (Value -> Value)
	| VNeutral Neutral

data Neutral =
	NFree Name		--variabile
	| NApp Neutral Value	--applicazione termine - valore

vfree :: Name -> Value --creo valore corrispondente ad una variabile libera
vfree n = VNeutral (NFree n)

type Env = [Value]

eval_up :: Term_up -> Env -> Value
eval_up (Ann e _ ) d = eval_down e d
eval_up (Free x) d = vfree x
eval_up (Bound i) d = d !! i
eval_up (e :@: e') d = vapp (eval_up e d) (eval_down e' d)

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n ) v = VNeutral (NApp n v)

eval_down :: Term_down -> Env -> Value
eval_down (Inf i) d = eval_up i d
eval_down (Lam e) d = VLam (\x -> eval_down e (x:d))

data Kind = Star deriving (Show)

data Info =
	HasKind Kind
	| HasType Type
	deriving (Show)

type Context = [(Name, Info)]

--type checking
type Result a = Either String a --result monad

kind_down :: Context -> Type -> Kind -> Result ()
kind_down gamma (TFree x) Star = 
	case lookup x gamma of
		Just (HasKind Star) -> return ()
		Nothing -> error "unknown identifier"

kind_down gamma (Fun k k') Star =
	do
		kind_down gamma k Star
		kind_down gamma k' Star

type_up0 :: Context -> Term_up -> Result Type
type_up0 = type_up 0


type_up :: Int -> Context -> Term_up -> Result Type
type_up i gamma (Ann e tau) =
	do
		kind_down gamma tau Star
		type_down i gamma e tau
		return tau

type_up i gamma (Free x) =
	case lookup x gamma of
		Just (HasType tau) -> return tau
		Nothing -> error "unknown identifier"

type_up i gamma (e :@: e') =
	do
		sigma <- type_up i gamma e
		case sigma of
			Fun tau tau' -> do
						type_down i gamma e' tau
						return tau'
			_ -> error "illegal application"

type_down :: Int -> Context -> Term_down -> Type -> Result ()
type_down i gamma (Inf e) tau =
	do 
		tau' <- type_up i gamma e
		unless (tau == tau') (error "type mismatch")

type_down i gamma (Lam e) (Fun tau tau') =
	type_down (i+1) ((Local i, HasType tau) : gamma)
		(subst_down 0 (Free (Local i)) e) tau'

type_down i gamma _ _ = error "type mismatch"

subst_up :: Int -> Term_up -> Term_up -> Term_up
subst_up i r (Ann e tau) = Ann (subst_down i r e) tau
subst_up i r (Bound j) = if i==j then r else Bound j
subst_up i r (Free y) = Free y
subst_up i r (e :@: e') = subst_up i r e :@: subst_down i r e'

subst_down :: Int -> Term_up -> Term_down -> Term_down
subst_down i r (Inf e) = Inf (subst_up i r e)
subst_down i r (Lam e) = Lam (subst_down (i+1) r e)


--quotation
quote0 :: Value -> Term_down
quote0 = quote 0

quote :: Int -> Value -> Term_down
quote i (VLam f) = Lam (quote (i+1) (f(vfree(Quote i))))
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> Term_up --quota gli argomenti
neutralQuote i (NFree x) = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

--boundfree controlla se la variabile che occorre in testa alla applicazione
--è una Quote e quinti una variabile legata o un free name
boundfree :: Int -> Name -> Term_up
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x = Free x


id' = Lam (Inf (Bound 0))
const' = Lam ( Lam (Inf (Bound 1)))

tfree a = TFree (Global a)
free x = Inf (Free (Global x))

term1 = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"
term2 = Ann const' (Fun (Fun (tfree "b") (tfree "b"))
			(Fun (tfree "a")
				(Fun (tfree "b") (tfree "b"))))
	:@: id' :@: free "y"

env1 = [(Global "y", HasType (tfree "a")),
	(Global "a", HasKind Star)]

env2 = [(Global "b", HasKind Star)] ++ env1

