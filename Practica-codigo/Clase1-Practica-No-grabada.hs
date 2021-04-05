{- 
-pattern matching
-tabulación
-esPar
-esImpar
-es diferente programar una funcion como f 2 = 0 (entrada y salida),otra forma de ver una funcion es como que lo de entrada comvierte la -entrada a cualquier otra cosa como Int --> Bool
-esPar 2 ---> SI / NO
-}
--------------------------------
--sumar n m = n + m
--otra forma sumar n m = (+) n m
-------------------------------- 
esPar :: Int -> Bool
esPar n =  (n `mod` 2) == 0
-- no hace falta escribir (10 `mod` 2) puede ser (mod 10 2) 
--------------------------------

--Planteo del profesor (Clase grabada)
esPar2 n :: Int -> Bool
esPar2 n | (mod n 2 == 0) = True
         | otherwise = False

{-
Pattern matching:
    f :: Int -> Int (Fibonacci)
        f 0 = 1
        f 1 = 1
        f n = f (n-1) + f (n-2)
-}

