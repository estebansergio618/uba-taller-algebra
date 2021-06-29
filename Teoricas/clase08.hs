-- Combinatoria
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial(n-1)
-----------------------------------------------------------
-- Ejercicio 1:
combinatorio :: Integer -> Integer -> Integer
combinatorio n k = (factorial n) `div` ((factorial k)*(factorial (n-k)))

combinatorio1 :: Integer -> Integer -> Integer
combinatorio1 _ 0 = 1
combinatorio1 n k
    | n == k    = 1
    | otherwise = c
    where c = (combinatorio1 (n-1) k) + (combinatorio1 (n-1) (k-1))
-------------------------------------------------------------------------
-- Combinatorio pensado con la wiki de la materia
-- Un poco más eficiente
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
--------------------------------------------------------------------------
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
-- Ejercicio de la Segunda Lista(Triangulo de Pascal)
pascal :: Integer -> Integer -> Integer
pascal _ 0      = 1
pascal n k
    | n == k    = 1
    | otherwise = pascal (n-1) k + pascal (n-1) (k-1)

{-
 
        1
        1 1
        1 2 1
        1 3 3 1
        1 4 6 4 1
        ....

        C(0,0)
        C(1,0) C(1,1)
        C(2,0) C(2,1) C(2,2)
        C(3,0) C(3,1) C(3,2) C(3,3)
        C(4,0) C(4,1) C(4,2) C(4,3) C(4,4)
        C(5,0) C(5,1) C(5,2) C(5,3) C(5,4) C(5,5)


        fila n    ---> [C(n,0), C(n,1), C(n,2), ..., C(n,m), ....C(n,n)]
        columna n ---> [C(n,m), C(n+1,m), C(n+2,m), ..., C(n,m)]
-}

{-binomial :: Integer -> Integer -> Integer
binomial n k = factorial n `div` (factorial k * factorial (n - k))
-}
fila :: Integer -> [Integer]
fila n = filaAPartirDe n 0       

filaAPartirDe :: Integer -> Integer -> [Integer]
filaAPartirDe n m 
    | n < m          = []
    | otherwise      = binomial n m : filaAPartirDe n (m + 1)

-- columna :: Integer -> [Integer]
-- columna n = columnaAPartirDe 0 n       

-- columnaAPartirDe :: Integer -> Integer -> [Integer]
-- columnaAPartirDe n m
--     | n < m          = []
--     | otherwise      = binomial n m : filaAPartirDe (n+1) m
--------------------------------------------------------------------
-- Pensemos en el ejemplo de fibonacci para hacer eficiente el código
-- Sabemos que el combinatorio n k = n*(n-1)*(n-2)*....*(n-(k-1))/k!

-- productoParaCombinatorio :: Integer -> Integer -> Integer
-- productoParaCombinatorio 0 _ = 1
-- productoParaCombinatorio n k = n*(productoParaCombinatorio (n-1) k)

-- combinatorioS :: Integer -> Integer -> Integer
-- combinatorioS n k = (productoParaCombinatorio n k) `div` (factorial k)
---------------------------------------------------------------------------



