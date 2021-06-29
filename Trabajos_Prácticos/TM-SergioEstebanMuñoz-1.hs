-- Ejercicio 1:
-- Escribir la función:
-- satisfaceGoldbach :: Integer -> Bool
-- que recibe un número natural n y devuelve True si y solo sí el n es par, mayor
-- que 2 y suma de dos números primos o False en caso contrario


--ES PAR
esParMayor2 :: Integer -> Bool
esParMayor2 n = (n > 2) && n `mod` 2 == 0 

--ES PRIMO
-- menorDivisor :: Integer -> Integer
-- menorDivisor n = menorDivisorDesde n 2

-- menorDivisorDesde :: Integer -> Integer -> Integer
-- menorDivisorDesde n k | n `mod` k == 0 = k
--                       | otherwise      = menorDivisorDesde n (k+1)

-- esPrimo :: Integer -> Bool
-- esPrimo 1 = False
-- esPrimo n = n == menorDivisor n

existeUn :: Integer -> Integer -> Bool
existeUn n m 
  | n < m                               = False
  | m > floor(sqrt(fromInteger n))      = True
  | n `mod` m == 0                      = False
  | otherwise                           = existeUn n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = existeUn n 2

-- ES SUMA DE DOS PRIMOS
criterio :: Integer -> Integer -> Bool
criterio n 1 = False
criterio n k | esPrimo k && esPrimo (n-k) = True            
             | otherwise                  = criterio n (k-1) 

sumaDeDosPrimos :: Integer -> Bool
sumaDeDosPrimos n = criterio n n

-- SATISFACE GOLDBACH 
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n = esParMayor2 n && sumaDeDosPrimos n

-- Ejercicio 2
-- Escribir la función
-- verificarConjeturaHasta :: Integer -> Bool
-- que recibe un número natural n par mayor que 2 y devuelve True si y solo sí
-- la conjetura es cierta para todos los naturales pares mayores que 2 y menores o
-- iguales que n o False en caso contrario

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta 4 = True
verificarConjeturaHasta n = satisfaceGoldbach n && verificarConjeturaHasta (n-2) == True

-- Ejercicio 3
-- Escribir la función
-- descomposicionEnPrimos :: Integer -> (Integer,Integer)
-- que recibe un número natural n par mayor que 2 y devuelve un par ordenado
-- (a,b) de números primos tales que a + b == n.

criterio1 :: Integer -> Integer -> (Integer,Integer)
criterio1 n k | esPrimo k && esPrimo (n-k) == True = (k,n-k)            
              | otherwise                          = criterio1 n (k-1) 

descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n = criterio1 n n

-- Ejercicio 4
-- Escribir la función
-- numeroDeDescomposiciones :: Integer -> Integer
-- que recibe un número natural n par mayor que 2 y devuelve la cantidad de pares
-- ordenados (a, b) de números primos tales que a + b == n.
-- *Main> numeroDeDescomposiciones 88
-- 8
-- *Main> numeroDeDescomposiciones 10
-- 3
-- *Main> numeroDeDescomposiciones 4
-- 1
-- *Main> numeroDeDescomposiciones 123456
-- 2904

numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = contador n n  

contador ::Integer -> Integer -> Integer
contador n 1   =  0
contador n k   | esPrimo k && esPrimo (n-k) = 1 + contador n (k-1)
               | otherwise                  = contador n (k-1)


