-- =======================Alumno: Sergio Esteban===================================

-- En algunos problemas use la función de haskell reverse y (++) pero posteriormente en los ejericios 13 y 14 las defino--
-- Ejercicio 1 :
-- Que devuelve la productoria de los elementos.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x*(productoria xs)

-- Ejercicio 2:
-- Que dado un número N y una lista xs, suma N a cada
-- elemento de xs.

sumarN :: Int -> [Int] -> [Int] 
sumarN _ [] = []
sumarN n (x:xs) = (n + x) : sumarN n xs

-- Ejercicio 3:
-- Que dada una lista no vacía xs, suma el primer
-- elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]
 
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero []     = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- Ejercicio 4:
-- Que dada una lista no vacÍa xs, suma el último
-- elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo l  = sumarN (head (reverse l)) l

-- Ejercicio 5:
-- Que devuelve una lista con los elementos pares de la lista
-- original. Ejemplo pares [1,2,3,5,8] [2,8]

pares :: [Int] -> [Int] 
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise    = pares xs

-- Ejercicio 6:
-- Que elimina la primera aparición del elemento en la
-- lista (de haberla).

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
           | otherwise   = x : quitar n xs 

-- Ejercicio 7:
-- Que elimina todas las apariciones del elemento
-- en la lista (de haberla).

quitarTodos :: Int -> [Int] -> [Int]
quitarTodos _ [] = []
quitarTodos n (x:xs) 
  | n /= x    =  x : quitarTodos n xs
  | otherwise = quitarTodos n xs

-- Ejercicio 8:
-- -- Que indica si una lista tiene elementos repetidos
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y    = True 
    | otherwise = pertenece x ys

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False -- Si no hay ningun elemento, ninguno se puede repetir.
hayRepetidos (x:xs) 
  | x `pertenece` xs = True
  | otherwise        = hayRepetidos xs

-- Ejercicio 9:
-- Que deja en la lista la primera aparicíon
-- de cada elemento, eliminando las repeticiones adicionales.

-- Forma 1:
-- eliminarRepetidosAlFinal :: [Int] -> [Int]
-- eliminarRepetidosAlFinal [] = []
-- eliminarRepetidosAlFinal (x : xs)
--   | pertenece x xs     = x : eliminarRepetidosAlFinal (quitarTodos x xs)
--   | otherwise          = x : eliminarRepetidosAlFinal xs

-- Forma 2:
esta :: Int -> [Int] -> Bool
esta _ []     = False
esta n (x:xs) 
  | n == x    = True
  | otherwise = esta n xs

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs)
   | x `esta` xs    = eliminarRepetidosAlFinal xs
   | otherwise      = x:eliminarRepetidosAlFinal xs

-- Ejercicio 10:
-- eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la última aparición
-- de cada elemento, eliminando las repeticiones adicionales.

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio []     = []
eliminarRepetidosAlInicio (x:xs) = reverso (reverso (eliminarRepetidosAlFinal (x:xs)))

-- Ejercicio 11:
-- maximo :: [Int] -> Int que calcula el máximo elemento de una lista no vacía.

maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs) 
  | x > maximo xs = x
  | otherwise     = maximo xs

-- Ejercicio 12:
-- ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = m : ordenar (quitar m xs)
            where m = menor xs

menor ::[Int] -> Int
menor [x]     = x
menor (x:xs)
  | x < m     = x
  | otherwise = m
  where m = menor xs

-- Ejercicio 13:
-- reverso :: [Int] -> [Int] que dada una lista invierte su orden.

reverso :: [Int] -> [Int]
reverso []     = []
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio 14:
-- concatenar :: [Int] -> [Int] -> [Int] que devuelve la concatenación de la primera
-- lista con la segunda. Ejemplo concatenar [1,2,3] [4,5,6] [1,2,3,4,5,6],
-- concatenar [] [4,5,6] [4,5,6]. Esta operaci´on est´a en el prelude y se escribe como
-- (++).

-- concatenar [1,2,3] [4,5,6] = [1,2,3,4,5,6]
concatenar :: [a] -> [a] -> [a]
concatenar []       q = q
concatenar (x : xs) q = x : concatenar xs q


-- Ejercicio 15:
-- zipi :: [a] -> [b] -> [(a,b)] que devuelve una lista de tuplas, cada tupla contiene
-- elementos de ambas listas que ocurren en la misma posición. En caso que tengan distintas
-- longitudes, la longitud de la lista resultado es igual a la longitud de la lista m´as chica
-- pasada por par´ametro. Ejemplo zipi [1,2,3] ['a','b','c']
-- [(1,'a'), (2,'b'), (3,'c')], zipi [1,2,3] ['a','b'] [(1,'a'), (2,'b')]. Esta
-- operación est´a en el prelude y se escribe como zip.

zipi :: [a] -> [b] -> [(a,b)]
zipi [] ys            = []
zipi xs []            = []
zipi (x:xs) (y:ys)    = (x,y) : zipi xs ys


