--
-- LAMBDA CALCOLO + EAGER SEMANTICA OPERAZIONALE
--

--proviamo ad estendere lambda con il linguaggio eager!!!
--ora estendiamo con il sistema di tipi!!!!
module LamUntiped where
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer
import System.IO

import Debug.Trace

import Syntax

--evaluation
data Value
  = VInt Integer
  | VClosure String Expr (LamUntiped.Scope)
  | VPair Value Value

instance Show Value where
  show (VInt x) = show x
  show (VClosure str expr env) = "<<closure " ++ str ++ " " ++ show expr ++ "{" ++ mostra env ++"}" ++ ">>"
  show (VPair a b) = "(" ++ (show a) ++ " , " ++ (show b) ++ ")"

data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = (depth s) + 1 } -- modify :: (s -> s) -> State s ()
  out <- m
  modify $ \s -> s { depth = (depth s) - 1 }
  return out

red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d, x)] --tell produce l'output, writer monad class (Monoid w, Monad m) => MonadWriter w m | m -> w where tell :: w -> m () 
  return ()

type Step = (Int, Expr)
type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value

mostra :: LamUntiped.Scope -> String
mostra scope = show (Map.toList scope)

--sostituisco nel primo termine le occorrenze del terzo con il secondo
-- body[ expression / var]
riscrittura :: Expr -> Expr -> Expr -> Expr
riscrittura body expr var@(Var v) =
 case body of
  Lit (LInt int) -> Lit (LInt int)
  Lit (LPair lx rx) -> Lit (LPair (riscrittura lx expr var) (riscrittura rx expr var))
  App e1 e2 -> App (riscrittura e1 expr var) (riscrittura e2 expr var)
  Lam name e ->
   if name == v 
   then Lam name e
   else Lam name (riscrittura e expr var)
  Syntax.Sum e1 e2 -> Syntax.Sum (riscrittura e1 expr var) (riscrittura e2 expr var)
  Sub e1 e2 -> Sub (riscrittura e1 expr var) (riscrittura e2 expr var)
  Mul e1 e2 -> Mul (riscrittura e1 expr var) (riscrittura e2 expr var)
  IfThenElse e1 e2 e3 -> IfThenElse (riscrittura e1 expr var) (riscrittura e2 expr var) (riscrittura e3 expr var)
  Syntax.First e -> Syntax.First (riscrittura e expr var)
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
  e -> error ("riscrittura:ERROR " ++ show e)
  
riscrittura _b _e _notvar = error "riscrittura su un temine non variable, non prevista"

eval :: LamUntiped.Scope -> Expr -> Eval Value
eval env expr = case expr of

  Lit (LInt x) -> do
    return $ VInt (fromIntegral x)

  Lit (LPair a b) -> do
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
    VInt y <- eval env t2 --se tutto va secondo i piani mi torna un intero? VInt
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
    value <- eval env b --per ora usiamo gli int
    case value of
     VInt v -> 
      case v of
       0 -> 
        eval env t1
       _ ->
        eval env t2
     result -> error ("IFTHENELSE: ERRO on" ++ (show $ IfThenElse b t1 t2))

  Syntax.First a -> inc $ do
    VPair f s <- eval env a --casini se non c'è un pair!!
    return f

  Second  a -> inc $ do
    VPair f s <- eval env a --casini se non c'Ã¨ un pair!!
    return s

--extend inserisce nello scope
extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0) 
--FINE EVAL

-- poter visualizzare al meglio una closure!

v2e :: Value -> Expr
v2e (VInt n) = (Lit (LInt n))
v2e (VPair v1 v2) = (Lit (LPair (v2e v1) (v2e v2)))
v2e (VClosure str exp scope) = (Lam str  (vClosure2expr exp (Map.delete str scope) ))
v2e _ = error "V2E error"

vClosure2expr :: Expr -> LamUntiped.Scope -> Expr
vClosure2expr l@(Lit _) env = l
vClosure2expr (Lam n expr) env = (Lam n (vClosure2expr expr (Map.delete n env)))
vClosure2expr (Var name) env =
        case (Map.member name env) of
                True -> v2e (env Map.! name)
                False -> (Var name)
vClosure2expr (App e1 e2) env = (App (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Syntax.Sum e1 e2) env = (Syntax.Sum (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Syntax.Sub e1 e2) env = (Syntax.Sub (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Mul e1 e2) env = (Mul (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (IfThenElse e1 e2 e3) env = (IfThenElse (vClosure2expr e1 env) (vClosure2expr e2 env) (vClosure2expr e3 env))
vClosure2expr (Syntax.First e) env = (Syntax.First (vClosure2expr e env))
vClosure2expr (Second e) env = (Second (vClosure2expr e env))
vClosure2expr (LetIn name e1 e2) env = (LetIn name (vClosure2expr e1 env) (vClosure2expr e2 env))--let n=a in b == (\a.b)e
vClosure2expr (Fix expr) env = (Fix (vClosure2expr expr env))
vClosure2expr expr scope = expr

