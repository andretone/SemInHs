module Approximations where
import qualified Data.Map as Map
import Syntax hiding (LPair)

{-
Approssimazioni:
possono essere numeri interi, 
oppure liste di coppie che descrivono 
il comportamento di una funzione
-}

data Approximation = 
 N Integer |
 F [(Approximation,Approximation)]
 deriving (Eq)

--per ora omettiamo le coppie!!
instance Show Approximation where
 show (N int) = "n:" ++ (show (n2Int int))
 show (F a) = "fun:" ++ show a
{-
Funzione ausiliaria per "tagliare" le liste e visualizzare solo
una porzione della lista infinita
-}
filter2show :: Int -> Approximation -> Approximation
filter2show int (N i) = N i
filter2show int (F ax) = F 
 (filter 
  (filterBottoms) 
  ( map 
     (\(sx,dx) -> ((filter2show int sx) ,(filter2show int dx)))
     (take int ax)
  )
 )
 where
  filterBottoms :: (Approximation,Approximation) -> Bool
  filterBottoms ( _ , (F [] ) ) = False
  filterBottoms  _  = True


--interi enumerati

int2Enum :: Integer -> Approximation
int2Enum int = N $ int2N int

lint2N :: Expr -> Approximation
lint2N (LInt n) = int2Enum n
lint2N _ = error "wrong argument"

{-
enumerare gli interi, dato un naturale
torno un intero in Z
-}
n2Int :: Integer -> Integer
n2Int n
 | n >= 0 && odd n = (-(n+1))`div`2
 | n >= 0 && even n = n`div`2
 | otherwise = error "n2Int wrong argument"

int2N :: Integer -> Integer
int2N int
 | int < 0 = (int*(-2))-1
 | int >= 0 = int*2

enumSum :: Approximation -> Approximation -> Approximation
enumSum (N a) (N b) =
 N $ int2N $ ((n2Int a) + (n2Int b))
enumSum _ _ = error "wrong sum"

enumSub :: Approximation ->Approximation ->Approximation
enumSub (N a) (N b) =
 N $ int2N $ ((n2Int a) - (n2Int b))
enumSub _ _ = error "wrong sub"

enumMul :: Approximation ->Approximation ->Approximation
enumMul (N a) (N b) =
 N $ int2N $ ((n2Int a) * (n2Int b))
enumMul _ _ = error "wrong mul"

{-
Environment, una Map, (come negli altri moduli)
-}
type Environment = Map.Map Name Approximation

emptyEnv :: Environment
emptyEnv = Map.empty

insertEnv :: Name -> Approximation -> Environment -> Environment
insertEnv = Map.insert

member :: Name -> Environment -> Bool
member = Map.member

modifyEnv :: Approximation -> Name -> Environment -> Environment
-- modifyEnv v x rho = rho[v/x]
modifyEnv v x rho = Map.insert x v rho

--proviamo a rappresentare alcune funzioni

approx :: Expr -> Environment -> Approximation
approx (LInt n) = \e -> (int2Enum n)

approx (Var name) = \e -> if member name e then (e Map.! name) else F []
--il bottom come una lista vuota: F []

approx (Sum e1 e2) = \e -> pluslift (approx e1 e) (approx e2 e)
 where
  pluslift aa@(N a) bb@(N b) = enumSum aa bb
  pluslift _ _ = F [] --caso in cui non posso fare la somma

approx (Sub e1 e2) = \e -> sublift (approx e1 e) (approx e2 e)
 where
  sublift aa@(N a) bb@(N b) = enumSub aa bb
  sublift _ _ = F [] --caso in cui non posso fare la sottrazione

approx (Mul e1 e2) = \e -> mullift (approx e1 e) (approx e2 e)
 where
  mullift aa@(N a) bb@(N b) = enumMul aa bb
  mullift _ _ = F [] --caso in cui non posso fare la mul

{-
Per una lambda astrazione la sua approssimazione è una lista di coppie,
come primo elemento di ogni coppia c'è un intero, come secondo l'approssimazione
della funzione con in input l'intero (primo elemento della coppia), 
quindi non faccio altro che aggiungere all'environment la variabile x con valore
il nuovo intero e calcolare l'approssimzione.

La laziness di haskell ci permette di poter costruire queste liste infinite
e di non dover calcolare il valore per ogni coppia se non strettamente
necessario.
-}
approx (Lam name expr) = \e -> F $ map ((funzioncina)e) [0..]
 where funzioncina = \e -> \n -> ( N n , ((approx expr) (insertEnv name (N n) e)) )

{-
L'applicazione non fa altro che selezionare il giusto elemento dalla lista
approssimazione del primo termine, come chiave di ricerca utilizza il primo 
elemento della coppia di ogni elemento.
Il risultato è il secondo elemento della coppia selezionata.
-}
approx (App t1 t2) = \e -> searchAprx (approx t1 e) (approx t2 e)
 where
  searchAprx :: Approximation -> Approximation -> Approximation
  searchAprx (F ((a, y):ax)) x =
   if a == x then y else searchAprx (F ax) x
  searchAprx (F []) _ = F []
  searchAprx _ (F []) = F []

approx (IfThenElse e1 e2 e3) = \e -> (cond (approx e1 e) e2 e3) e
 where
  cond :: Approximation -> Expr -> Expr -> Environment -> Approximation
  cond (F _) _ _ = \e -> F []
  cond (N n) th el 
   | n == 0 = approx th
   | n /= 0 =  approx el
   | otherwise = \e -> F []

approx (LetIn name e1 e2) = \e -> ((approx e2) (insertEnv name ((approx e1) e) e))

approx (Rec name (Lam x t)) = \e -> 
 (approx (Lam x t) (insertEnv name (approx (Rec name (Lam x t)) e) e) )

approx _ = error "not implemeted yet"


{-
Calcola approssimazioni di un rec!
muuu prende in input un termine nella forma:
Rec ..

restituisce una lista delle sue approssimazioni.
La prima è bottom, poi itera applicando al termine l'approssimazione
precedentemente calcolata.

Questa funzione può essere utile per il calcolo dei punti fissi.
-}
muuu :: Expr -> Environment -> [ Approximation ]
muuu (Rec y (Lam x t)) e =
 iterate ff (F [])
 where
  ff = \fi -> approx (Lam x t) (insertEnv y fi e) --fi e' l'approssimazione precedente
muuu _ _ = error "muuu accepts only Rec-terms"
--Anche un metodo per visualizzare le approssimazioni, 
--scartando chi ha nel lato dx un bel bottom.
