-- Sergio Esteban - UBA
-- Lista Extra

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- 1) Escriba una función dígito :: Int -> Integer -> Integer tal que la expresión dígito i n calcule el i-ésimo dígito decimal del entero n, asumiendo que n es un entero no negativo y que i es un entero positivo.
-- *Main> dígito 1 12345
-- 5
-- *Main> dígito 3 12345
-- 3
-- *Main> dígito 5 12345
-- 1
-- *Main> dígito 10 12345
-- 0

digito :: Int -> Integer -> Integer
digito i n = (n `div` 10^(i-1)) `mod` 10

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- 2) Escriba una función sumaDeDígitos :: Integer -> Integer tal que la expresión sumaDeDígitos n
-- sea la suma de los dígitos del entero n, que asumimos es no negativo.
-- *Main> sumaDeDígitos 12345
-- 15
-- *Main> sumaDeDígitos 8
-- 8
-- *Main> sumaDeDígitos 0
-- 0

sumaDeDigitos1 :: Integer -> Integer -> Integer
sumaDeDigitos1 1 n = digito 1 n
sumaDeDigitos1  m n = sumaDeDigitos1 (m-1) n + digito (fromIntegral m) n

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos 0 = 0
sumaDeDigitos n = sumaDeDigitos1 (n-1) n + digito (fromIntegral n) n 

-- NOTA: Con 6 a + digitos el programa se demora mucho en calcularlo

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- 3) Escriba una función dígitoMáximo :: Integer -> Integer tal que la expresión dígitoMáximo n
-- sea dígito más grande de n, que asumimos es no negativo.
-- *Main> dígitoMáximo 123454321
-- 5
-- *Main> dígitoMáximo 10001
-- 1
-- *Main> dígitoMáximo 8
-- 8
-- *Main> dígitoMáximo 0
-- 0

-- digitoMaximo :: Integer -> Integer

----- Necesito una funcion cantidad de digitos

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- 4) Escriba una función factorial :: Integer -> Integer tal que factorial n sea el factorial del entero no negativo n
-- *Main> factorial 10
-- 3628800
-- *Main> factorial 1
-- 1
-- *Main> factorial 0
-- 1

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n*(factorial (n-1))
            | otherwise = undefined
            
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------   

-- 5) Escriba una función e_approx :: Integer -> Double tal que para cada entero no negativo n el valor de e_approx n sea sum (k=0,..,n) 1/k!
-- *Main> e_approx 10
-- 2.7182818011463845
-- *Main> e_approx 2
-- 2.5
-- *Main> e_approx 1
-- 2.0
-- *Main> e_approx 0
-- 1.0

e_approx :: Integer -> Double
e_approx 0 = 1
e_approx n = e_approx (n-1) + 1/fromIntegral (factorial n)

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- 7) Escribir una funcion pi_approx :: Integer -> Double que aproxime el numero pi como esta en la lista
-- Main> pi_approx 1000
-- 3.1415926533405423
-- *Main> pi_approx 10
-- 3.1414067184965018
-- *Main> pi_approx 1
-- 3.1666666666666665
-- *Main> pi_approx 0
-- 3.0

sumaAlternada :: Integer -> Double
sumaAlternada 1 = 1/24
sumaAlternada n = sumaAlternada (n-1) + (-1)^(n+1)/fromIntegral(2*n*(2*n+1)*(2*n+2))

pi_approx :: Integer -> Double
pi_approx 0 = 3
pi_approx n = 3 + 4*(sumaAlternada n)

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------