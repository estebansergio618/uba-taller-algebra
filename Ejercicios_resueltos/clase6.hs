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

eliminarRepetidosAlFinal :: [Int] -> [Int]



