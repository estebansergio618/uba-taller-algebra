-- Ejercicios Resueltos
-- Recordando funciones de clase
-------------------------------------------------------------------------
binomial :: Integer -> Integer -> Integer
binomial n m = producto n (n-m+1) `div` producto m 1

producto :: Integer -> Integer -> Integer
producto a b
    | a < b     = 1
    | otherwise = a * producto (a-1) b
-------------------------------------------------------------------------
type Set a = [a]
vacio :: Set a
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x ==  y    = True 
    | otherwise = pertenece x ys

agregar :: Eq a => a -> Set a -> Set a
agregar x c
    | pertenece x c = c
    | otherwise = x:c

union :: Eq a => Set a -> Set a -> Set a 
union [] c          = c
union (x:xs) c      = union xs (agregar x c)

agregarElementoAdelante :: Int ->  Set [Int] -> Set [Int]
agregarElementoAdelante _ []       = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _     = []
agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c ) (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))
--------------------------------------------------------------------------
-- Ejercicio 1:
-- Todas las formas de ubicar n bolitas numeradas en k cajas.
-- Nota: Noto que es una interpretación similar a la que vimos en clase
-- solo ahora me dan la longitud de la lista. 
-- Es decir, supongamos que tenemos 3 cajas numeradas del 1 al 3 y queremos meter
-- 2 bolitas numeradas pues seria lo mismo que usar variaciones [1,2,3] 2

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones (listasHasta k) n

listasHasta :: Int -> [Int]
listasHasta 1 = [1]
listasHasta k = listasHasta (k-1) ++ [k]