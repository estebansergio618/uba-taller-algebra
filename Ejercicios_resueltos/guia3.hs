-- Sergio Esteban - UBA
-- Clase: Recursión

-- Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div.
multiploDe3 :: Int -> Bool
multiploDe3 x | x == 3 = True
              | x < 3 = False
              | otherwise  = multiploDe3 (x-3)

-- Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n números impares. Ej: sumaImpares 3 -- 1+3+5 -- 9
sumaImpares :: Int -> Int
sumaImpares n | n == 0 = 0
              | n == 1 = 1
              | n > 1 = sumaImpares (n-1) + 2*n - 1
              |otherwise = undefined

-- Escribir una función medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · ·
medioFact :: Float -> Float 
medioFact 1 = 1
medioFact 2 = 2
medioFact n = n*medioFact(n-2)


-- Escribir una función que determine la suma de dígitos de un número positivo. Para esta función pueden utilizar div y mod
sumaDigitos :: Int -> Int
sumaDigitos n | 0 <= n && n < 10 = n
              | n >= 10 = sumaDigitos ( div n 10 ) + (mod n 10)
              | otherwise = undefined

-- Implementar una función que determine si todos los dígitos de un número son iguales

{-
unidades :: Integer -> Integer
unidades n = n `mod` 10
decenas n = (n `div` 10) `mod` 10
centenas n = (n `div` 100) `mod` 10
mil n = (n `div` 1000) `mod` 10
-}
digito :: Integer -> Integer -> Integer
digito i n = (n `div` 10^(i-1)) `mod` 10

todosIguales :: Integer -> Bool
todosIguales n | n < 10 = True
               | digito 1 n == digito 2 n = todosIguales (n `div` 10)
               | otherwise = False

