-- Sergio Esteban - UBA
-- Clase: Sumatorias

-- 1) Implementar una función g1 (i,n) = sum_(j=1,2,...,n) i^j
g1 :: Float -> Float -> Float
g1 i 1 = i
g1 i n = g1 i (n-1) + i**n

-- 2) Implementar una función  g2(n) = sum_(i=1,...,n) sum_(j=i,...,n) i^j
primeraSuma :: Double -> Double -> Double
primeraSuma i n | n == i = i**i
                | n > i = primeraSuma i (n-1) + i**n
                | otherwise = undefined

sumaDeSumas :: Double -> Double -> Double
sumaDeSumas 1 n = primeraSuma 1 n
sumaDeSumas m n = sumaDeSumas (m-1) n + primeraSuma n n

g2 :: Double -> Double
g2 n = sumaDeSumas (n-1) n + primeraSuma n n

-- 3) Implementar una función g3(n)= sum_(i=1 e i par) 2^i

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

g3 :: Int -> Int
g3 n | n == 0 = 0
     | (esPar n || False) == False = g3 (n-1)
     | otherwise = 2^n + g3 (n-1)

-- 4) Implementar una función que dado un n, sume todos los números naturales menores o iguales que n que tengan todos los dígitos iguales.
-- Apoyandome de la guia3.hs
digito :: Integer -> Integer -> Integer
digito i n = (n `div` 10^(i-1)) `mod` 10

todosIguales :: Integer -> Bool
todosIguales n | n < 10 = True
               | digito 1 n == digito 2 n = todosIguales (n `div` 10)
               | otherwise = False
-- Fin de apoyo de la guia3.hs

criterio :: Integer -> Integer
criterio n | n >= 1 && todosIguales n == True = n
           | otherwise = 0

sumaMenores :: Integer -> Integer
sumaMenores 1 = 1
sumaMenores n = sumaMenores (n-1) + criterio n
