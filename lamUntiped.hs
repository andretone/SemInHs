--proviamo ad estendere lambda con il linguaggio eager!!!
--ora estendiamo con il sistema di tipi!!!!
module Lam where
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer
import System.IO

--sintassi
type Name = (String) --vei a pagina 183-184 sui tipi delle variabili, esplicitati nel nome

--piazziamoci l'eager
--data Term =     X Var | --variabili x : tau
--                Num N | Sum Term Term | Sub Term Term | Mul Term Term | IfThenElse Term Term Term |
--                Pair Term Term | First Term | Second Term |
--                Function Var Term | Apply Term Term | LetIn Var Term Term |
--                Rec Var Var Term deriving Show
----

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Expr
  | Sum Expr Expr | Sub Expr Expr | Mul Expr Expr
  | IfThenElse Expr Expr Expr
  | First Expr | Second Expr
  | LetIn Name Expr Expr | Fix Expr | Rec Name Expr --in rec Expr deve essere Lam Name Expr
  deriving (Eq, Show)

data Lit --literals 
  = LInt Integer
  | LBool Bool
  | LPair Expr Expr
  deriving (Show, Eq ) --Ord

--evaluation
data Value
  = VInt Integer
  | VBool Bool --andrebbe tolto?
  | VClosure String Expr (Lam.Scope)
  | VPair Value Value

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
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

mostra :: Lam.Scope -> String
mostra scope = show (Map.toList scope)

eval :: Lam.Scope -> Expr -> Eval Value
eval env expr = case expr of

  Lit (LInt x) -> do
    return $ VInt (fromIntegral x)

  Lit (LBool x) -> do
    return $ VBool x

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

  Lam.Sum t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2 --se tutto va secondo i piani mi torna un intero? VInt
    return $ VInt (x+y)

  LetIn name e1 e2 -> eval env (App (Lam name e2) (e1))

  Fix v ->  eval env (App v (Lam "x" (App (Fix v) (Var "x")) ) )
  
 -- Rec ny l@(Lam nx t) -> eval env (Lam nx (eval enva t)) --problematico

  Sub t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2
    return $ VInt (x-y)
    
  Mul t1 t2 -> inc $ do
    VInt x <- eval env t1
    VInt y <- eval env t2
    return $ VInt (x*y)

  IfThenElse b t1 t2 -> inc $ do
    VInt v <- eval env b --per ora usiamo gli int
    case v of
      0 -> 
        eval env t1
      _ ->
        eval env t2

  Lam.First a -> inc $ do
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

vClosure2expr :: Expr -> Lam.Scope -> Expr
vClosure2expr l@(Lit _) env = l
vClosure2expr (Lam n expr) env = (Lam n (vClosure2expr expr (Map.delete n env)))
vClosure2expr (Var name) env =
        case (Map.member name env) of
                True -> v2e (env Map.! name)
                False -> (Var name)
vClosure2expr (App e1 e2) env = (App (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Lam.Sum e1 e2) env = (Lam.Sum (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Lam.Sub e1 e2) env = (Lam.Sub (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (Mul e1 e2) env = (Mul (vClosure2expr e1 env) (vClosure2expr e2 env))
vClosure2expr (IfThenElse e1 e2 e3) env = (IfThenElse (vClosure2expr e1 env) (vClosure2expr e2 env) (vClosure2expr e3 env))
vClosure2expr (Lam.First e) env = (Lam.First (vClosure2expr e env))
vClosure2expr (Second e) env = (Second (vClosure2expr e env))
vClosure2expr (LetIn name e1 e2) env = (LetIn name (vClosure2expr e1 env) (vClosure2expr e2 env))--let n=a in b == (\a.b)e
vClosure2expr (Fix expr) env = (Fix (vClosure2expr expr env))
vClosure2expr expr scope = expr

--TESTS
main = putStrLn $ show $ v2e $ fst $ runEval anothertest

anothertest =(Lam "q" (App (Lam "a" (Lam "b" (App (Var "b") (Var "a")))) (App (Lam "z" (Lam "c" (Var "c"))) (Lit (LInt 1)) ) ) )

test = (App (Lam "y" (App (Var "s") (Var "y"))) (Lit (LInt 1)) )

controversial = (App (Lam "y" (Lam "x" (App (Var "y") (Var "x")) )) (Var "x")) 

tt = (Lam "x" (Lam "y" (App (Lam "x" (App (Var "x")(App (Var "x")(Var "y")))) (Lam "z" (App (Var "z")(Var "x")))) ))

verytest =
	(App
		(Lam
			"x"
                        (App
                          (Lam
                            "y"
                            (Lam "x" (App (Var "y") (Var "x" ))) )
                          (Var "x")
                        )
                )
                (Lam "e" (Lam.Sum (Var "e") (Lit(LInt 1))))
        )

verydifficult = 
		(App 
			(App 
				(Lam 
					"x" 
					(App 
						(Lam 
							"y"
							(Lam "x" (App (Var "y") (Var "x" ))) ) 
						(Var "x") 
					)
				)
				(Lam "e" (Lam.Sum (Var "e") (Lit(LInt 1))))
			) 
			(Lit (LInt 1))
		)

t0 = ( LetIn
         "x"
         (Lam "y" (Var "y"))
         (App (Var "x") (Lit (LInt 1)))
     )

t1 =	(Lam.Second (IfThenElse (Sub (Lit (LInt 2)) (App (Lam "x" (Lam.Sum (Lit (LInt 2)) (Var "x") ) ) (Lit (LInt 1)) ))
		(Lit (LInt 1))
		(Lit (LPair (Lit (LInt 11)) (Lit (LInt 12)) ))
	))

t2 = ( LetIn 
         "x" 
         (Lam "y" (Lam.Sum (Lit (LInt 2)) (Var "y")))
         (App (Var "x") (Lit (LInt 1))) 
     )

t3 = ( LetIn
         "x"
         (Lam.Sum (Var "x") (Var "x"))
         (Var "x")
     )

--VOGLIAMO AGGIUNGERE I LET? REC?
-- LetIn Var Expr Expr
--letIn :: Name -> Expr -> Expr -> Expr
--letIn str exp1 exp2 = (App (Lam str exp2) (exp1))

--fix v = v (\x . (fix v) x)
--fixed :: Expr -> Expr
--fixed v = (App v (Lam ("x",RecType) (App (fixed v) (Var ("x",RecType))) ) )
--fixed f = letIn "x" (App f (Var "x")) (Var "x")

fact' = (Lam "rec" (Lam "n" (IfThenElse (Var "n") (Lit (LInt 1)) (Mul (Var "n") (App (Var "rec") (Sub (Var "n") (Lit(LInt 1))))))))

testrec = App (Fix $ fact') (Lit (LInt 10))
