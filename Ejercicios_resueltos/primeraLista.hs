-- Sergio Esteban
-- Lista Extra

-- ========================================================================================
-- Ejercicio 1
digito :: Int -> Integer -> Integer
digito i n = (n `div` 10 ^ i) `mod` 10
-- ========================================================================================
-- Ejercicio 2
sumaDeDigitos1 :: Int -> Integer -> Integer
sumaDeDigitos1 0 n = digito 0 n
sumaDeDigitos1  m n = sumaDeDigitos1 (m-1) n + digito m n

numeroDeDigitos :: Integer -> Int
numeroDeDigitos 1 = 1
numeroDeDigitos n = 1 + numeroDeDigitos (n `div` 10)

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos 0 = 0
sumaDeDigitos n = sumaDeDigitos1 (numeroDeDigitos n - 1) n  

-- Otra forma:
--sumaDeDigitos :: Integer -> Integer
--sumaDeDigitos n 
--            | n > 0 = n `mod` 10 + sumaDeDigitos (n `div` 10)

-- ========================================================================================
-- Ejercicio 3:
digitoMaximo :: Integer -> Integer
digitoMaximo 0                                  = 0
digitoMaximo n 
    | unidad > digitoMaximo (numeroSinLaUnidad) = unidad
    | otherwise                                 = digitoMaximo (numeroSinLaUnidad)
    where unidad            = n `mod` 10 
          numeroSinLaUnidad = n `div` 10
-- ========================================================================================
-- Ejercicio 4:
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial(n-1)
-- ========================================================================================
-- Ejercicio 5:
e_approx :: Integer -> Double
e_approx 0 = 1
e_approx n = e_approx (n-1) + 1/fromIntegral (factorial n)
-- ========================================================================================
-- 6) Escriba una función unos :: Integer -> Integer tal que para cada entero no negativo n el
-- valor de unos n sea la cantidad de dígitos 1 en la escritura binaria de n.
-- *Main> unos 12345
-- 6
-- *Main> unos (2^6 - 1)
-- 6
-- *Main> unos 2^6
-- 1
-- *Main> unos 0
-- 0
-- ========================================================================================
-- Ejercicio 7:
sumaAlternada :: Integer -> Double
sumaAlternada 1 = 1/24
sumaAlternada n = sumaAlternada (n-1) + (-1)^(n+1)/fromIntegral(2*n*(2*n+1)*(2*n+2))

pi_approx :: Integer -> Double
pi_approx 0 = 3
pi_approx n = 3 + 4*(sumaAlternada n)
-- ========================================FIN=============================================
