module Approximations where
import qualified Data.Map as Map
import Syntax hiding (Lam)
import Data.List
import Debug.Trace
{-
Approssimazioni:
possono essere numeri interi, 
oppure liste di coppie che descrivono 
il comportamento di una funzione
-}

data Approximation = 
 N Integer |
 C Approximation Approximation |
 F [(Approximation,Approximation)]
 deriving (Eq)

--per ora omettiamo le coppie!!
instance Show Approximation where
 show (N int) = "n:" ++ (show int)
 show (C a1 a2) = "c:(" ++ show a1 ++ "," ++ show a2 ++ ")"
 show (F a) = "fun:" ++ show a
{-
Funzione ausiliaria per "tagliare" le liste e visualizzare solo
una porzione della lista infinita
-}
filter2show :: Int -> Approximation -> Approximation
filter2show int (N i) = N i
filter2show int (C a b) = C (filter2show int a)  (filter2show int b)
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

int2N :: Integer -> Integer --inverso, per ora non ci serve
int2N int
 | int < 0 = (int*(-2))-1
 | int >= 0 = int*2


--enumerare coppie
diagonals :: [(Integer, Integer)]
diagonals = [ (n-i, i)  | n <- [0..], i <- [0..n] ]

n2A :: Integer -> T -> Approximation
n2A n _ | n < 0 = error "wrong conversion n2Coulpe"
n2A n Num = N $ n2Int n
n2A n (Couple a b) = let (f , s) = diagonals !! (fromIntegral n) in C (n2A f a) (n2A s b)
n2A n Function = error "high order not supported"

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

denote' :: Expr -> Environment -> Approximation
denote' Bottom = \e -> F[]

denote' (LPair a b) = \e -> coupleLift (denote' a e) (denote' b e)
 where 
  coupleLift (F[]) _ = F []
  coupleLift _ (F[]) = F []
  coupleLift a b = C a b

denote' (LInt n) = \e -> (N n)

denote' (Var name) = \e -> if member name e then (e Map.! name) else F []
--il bottom come una lista vuota: F []

denote' (Sum e1 e2) = \e -> pluslift (denote' e1 e) (denote' e2 e)
 where
  pluslift (N a) (N b) = N $ a + b
  pluslift _ _ = F [] --caso in cui non posso fare la somma

denote' (Sub e1 e2) = \e -> sublift (denote' e1 e) (denote' e2 e)
 where
  sublift (N a) (N b) = N $ a - b
  sublift _ _ = F [] --caso in cui non posso fare la sottrazione

denote' (Mul e1 e2) = \e -> mullift (denote' e1 e) (denote' e2 e)
 where
  mullift (N a) (N b) = N $ a * b
  mullift _ _ = F [] --caso in cui non posso fare la mul

denote' (First a) = \e -> projectionFirstLift (denote' a e)
 where
  projectionFirstLift (C (F[]) _) = F []
  projectionFirstLift (C _ (F[])) = F []
  projectionFirstLift (C a b) = a
  projectionFirstLift  _ = F []

denote' (Second a) = \e -> projectionSecondLift (denote' a e)
 where
  projectionSeconfLift (C (F[]) _) = F []
  projectionSecondLift (C _ (F[])) = F []
  projectionSecondLift (C a b) = b
  projectionSecondLift  _ = F[]

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
denote' (Lam' name tipo expr) = \e -> F $ map ((funzioncina)e) [0..]
 where funzioncina = \e -> \n -> ( (n2A n tipo) , ((denote' expr) (insertEnv name (n2A n tipo) e)) )

{-
L'applicazione non fa altro che selezionare il giusto elemento dalla lista
approssimazione del primo termine, come chiave di ricerca utilizza il primo 
elemento della coppia di ogni elemento.
Il risultato è il secondo elemento della coppia selezionata.
-}
denote' (App t1 t2) = \e -> searchAprx (denote' t1 e) (denote' t2 e)
 where
  searchAprx :: Approximation -> Approximation -> Approximation
  searchAprx (F ((a, y):ax)) x =
   if a == x then y else searchAprx (F ax) x
  searchAprx (F []) _ = F []
  searchAprx _ (F []) = F []

denote' (IfThenElse e1 e2 e3) = \e -> (cond (denote' e1 e) e2 e3) e
 where
  cond :: Approximation -> Expr -> Expr -> Environment -> Approximation
  cond (F _) _ _ = \e -> F []
  cond (N n) th el 
   | n == 0 = denote' th
   | n /= 0 =  denote' el
   | otherwise = \e -> F []

denote' (LetIn name e1 e2) = \e -> ((denote' e2) (insertEnv name ((denote' e1) e) e))

denote' (Rec name (Lam' x tipo t)) = \e -> 
 (denote' (Lam' x tipo t) (insertEnv name (denote' (Rec name (Lam' x tipo t)) e) e) )

denote' _ = error "not implemeted yet"


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
muuu (Rec y (Lam' x tipo t)) e =
 iterate ff (F [])
 where
  ff = \fi -> denote' (Lam' x tipo t) (insertEnv y fi e) --fi e' l'approssimazione precedente
muuu _ _ = error "muuu accepts only Rec-terms"
--Anche un metodo per visualizzare le approssimazioni, 
--scartando chi ha nel lato dx un bel bottom.
