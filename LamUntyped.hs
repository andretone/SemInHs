--
-- LAMBDA CALCOLO + EAGER SEMANTICA OPERAZIONALE
--

--proviamo ad estendere lambda con il linguaggio eager!!!
module LamUntyped where
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer
import System.IO

import Debug.Trace

import Syntax hiding (Bottom)

--evaluation
data Value
  = VInt Integer
  | VClosure String Expr (LamUntyped.Scope)
  | VPair Value Value

instance Show Value where
  show (VInt x) = show x
  show (VClosure str expr env) = 
	"<<closure " ++ str ++ " " ++ show expr ++ "{" ++ mostra env ++"}" ++ ">>"
  show (VPair a b) = 
	"(" ++ (show a) ++ " , " ++ (show b) ++ ")"

{-
EvalState, inc, red, l'utilizzo del monad transformere WriteT, Step
Eval possono essere ignorati, non sono essenziali per l'implementazione 
attuale, ma solo per un eventuale controllo e debug.
-}

data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = (depth s) + 1 }
  out <- m
  modify $ \s -> s { depth = (depth s) - 1 }
  return out

red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d, x)]
  return ()

type Step = (Int, Expr)
type Eval a = WriterT [Step] (State EvalState) a

{-
Scope è il nostro environment, un dizionario,
nome variabile -> valore ad essa attribuito
-}
type Scope = Map.Map String Value

--funzione ausiliaria per mostrare il contenuto dello scope
mostra :: LamUntyped.Scope -> String
mostra scope = show (Map.toList scope)


{-
 riscrittura:
 sostituisce nel primo termine le occorrenze del terzo con il secondo
 body[ expression / var]
-}
riscrittura :: Expr -> Expr -> Expr -> Expr
riscrittura body expr var@(Var v) =
 case body of
  (LInt int) -> (LInt int)
  (LPair lx rx) -> 
   (LPair (riscrittura lx expr var) (riscrittura rx expr var))
  App e1 e2 -> 
   App (riscrittura e1 expr var) (riscrittura e2 expr var)
  Lam name e ->
   if name == v 
   then Lam name e
   else Lam name (riscrittura e expr var)
  Syntax.Sum e1 e2 -> 
   Syntax.Sum (riscrittura e1 expr var) (riscrittura e2 expr var)
  Sub e1 e2 -> 
   Sub (riscrittura e1 expr var) (riscrittura e2 expr var)
  Mul e1 e2 -> 
   Mul (riscrittura e1 expr var) (riscrittura e2 expr var)
  IfThenElse e1 e2 e3 -> 
   IfThenElse (riscrittura e1 expr var) (riscrittura e2 expr var) (riscrittura e3 expr var)
  Syntax.First e -> 
   Syntax.First (riscrittura e expr var)
  Second e -> Second (riscrittura e expr var)
  LetIn n e1 e2 ->
   (LetIn
    n
    (riscrittura e1 expr var)
    (if n == v
    then e2
    else (riscrittura e2 expr var))
   )
  Fix v -> Fix (riscrittura v expr var)
  Rec n e ->
   if n == v
   then Rec n e
   else Rec n (riscrittura e expr var)
  Var n ->
   if n == v
   then expr
   else Var n
  
riscrittura _b _e _notvar = 
 error "riscrittura su un temine non variable, non prevista"

{-
eval
dato lo scope attuale del termine, torna il suo valore
-}
eval :: LamUntyped.Scope -> Expr -> Eval Value
eval env expr = case expr of

  (LInt x) -> do
    return $ VInt (fromIntegral x)

  (LPair a b) -> do
    x <- (eval env a)
    y <- (eval env b)
    return $ VPair x y

  Var name -> do
    red expr
    return $ env Map.! name

  Lam name body -> inc $ do
    return (VClosure name body env)

  App a b -> inc $ do
    x <- eval env a
    red a
    y <- eval env b
    red b
    apply x y

  Syntax.Sum t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2
    return $ VInt (x+y)

  LetIn name e1 e2 -> eval env (App (Lam name e2) (e1))

  Fix v ->  eval env (App v (Lam "x" (App (Fix v) (Var "x")) ) )
  
  Rec ny l@(Lam nx t) -> return 
    (VClosure 
     nx 
     (riscrittura t (Rec ny l) (Var ny))
     env
    )

  Sub t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2
    return $ VInt (x-y)
    
  Mul t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2
    return $ VInt (x*y)

  IfThenElse b t1 t2 -> inc $ do
    value <- eval env b
    case value of
     VInt v -> 
      case v of
       0 -> 
        eval env t1
       _ ->
        eval env t2
     result -> error ("IFTHENELSE: ERROR on" ++ (show $ IfThenElse b t1 t2))

  Syntax.First a -> inc $ do
    VPair f s <- eval env a
    return f

  Second  a -> inc $ do
    VPair f s <- eval env a
    return s

{-
extend inserisce nello scope
una variabile con il suo valore
(se c'è già una variabile con lo stesso nome, 
la sostituisce
-}
extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

{-
applico ad una funzione (VClosure)
un valore, e continua la valutazione
-}
apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

--Scope vuoto
emptyScope :: Scope
emptyScope = Map.empty

{-
calcola il temine
(si ignori (EvalState 0), non è importante per la nostra implementazione)
-}
runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0) 
--FINE EVAL



{- 
poter visualizzare al meglio una closure!
torna a tradurre le VClosure in termini allo scopo di poterle visualizzare
-}
v2e :: Value -> Expr
v2e (VInt n) = (LInt n)
v2e (VPair v1 v2) = (LPair (v2e v1) (v2e v2))
v2e (VClosure str exp scope) = (Lam str  (vClosure2expr exp (Map.delete str scope) ))

vClosure2expr :: Expr -> LamUntyped.Scope -> Expr
vClosure2expr l@(LInt _) env = l
vClosure2expr l@(LPair _ _) env = l
vClosure2expr (Lam n expr) env = 
 (Lam n (vClosure2expr expr (Map.delete n env)))
vClosure2expr (Var name) env =
        case (Map.member name env) of
                True -> v2e (env Map.! name)
                False -> (Var name)
vClosure2expr (App e1 e2) env = 
 (App (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Syntax.Sum e1 e2) env = 
 (Syntax.Sum (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Syntax.Sub e1 e2) env = 
 (Syntax.Sub (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Mul e1 e2) env = 
 (Mul (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (IfThenElse e1 e2 e3) env = 
 (IfThenElse (vClosure2expr e1 env) (vClosure2expr e2 env) (vClosure2expr e3 env))
vClosure2expr (Syntax.First e) env = 
 (Syntax.First (vClosure2expr e env))
vClosure2expr (Second e) env = 
 (Second (vClosure2expr e env))
vClosure2expr (LetIn name e1 e2) env = 
 (LetIn name (vClosure2expr e1 env) (vClosure2expr e2 env))--let n=a in b == (\a.b)e
vClosure2expr (Fix expr) env = 
 (Fix (vClosure2expr expr env))
vClosure2expr expr scope = expr

