--TEST PER PROVARE IL CALCOLO DI UN PUNTO FISSO TRAMITE APPROSSIMAZIONI,
--FUNZIONI RICORSIVE

module Test where

import Control.Exception

g y = \x -> if x == 0 then 1 else x * y (x -1)

--main = putStrLn $ show $ ( (iterate g (undefined)) !! 4) 3

z :: Integer
z = undefined

main'' = do
 a <- try (evaluate ( ( (iterate g (undefined)) !! 4) 4  )) :: IO (Either SomeException Integer)
 case a of
  Left _ -> putStrLn "erorrino"
  Right a -> putStrLn $ show a

main' = do
 a <- try $ evaluate z :: IO (Either SomeException Integer)
 putStrLn $ show a



mumumu fun approx valore = do
 a <- try ( evaluate (fun approx valore) ) :: IO (Either SomeException Integer)
 case a of
  Left _ -> mumumu fun (fun approx) valore
  Right r -> return r

main = mumumu g undefined 10
