-- import Data.Numbers.Primes (primeFactors) 

-- Función que calcula los enteros del mcd(a,b)= a*x + b*y
mcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
mcdExt a b | b > a = mcdExt b a 
mcdExt a 0 = (a,0,1)
mcdExt a b = (d,s-t*k,t)
            where (r,k) = ( mod a b,div a b)
                  (d,t,s) = mcdExt b r 

--------------------------------------------------------------------------
-- . Una secuencia P_n es definida de la 
-- siguiente forma:
--     i)  P_1 = 2
--     ii) Para todo n >= 2, P_n es el mayor divisor primo de la expresión:
--             p_1*p_2*...*p_(n-1)+1.
-- Demostrar que P_n es diferente de 5.

-- este algoritmo me devuelve resultados de Pn evaluado hasta 8 que me da un
-- número muy grande y nunca pasa por el 5.

mayorDivisorPrimo :: Integer -> Integer
mayorDivisorPrimo = last . divisoresPrimos 

divisoresPrimos :: Integer -> [Integer]
divisoresPrimos 0 = []
divisoresPrimos 1 = []
divisoresPrimos n = m : divisoresPrimos (n `div` m)
  where m = menorDivisorPrimo n 
 
menorDivisorPrimo :: Integer -> Integer
menorDivisorPrimo x =
  head [y | y <- 2 : [3,5..(ceiling . sqrt . fromIntegral) x] ++ [x]
          , x `mod` y == 0]

paso1 :: Integer -> Integer -> Integer
paso1 b 1     = b
paso1 b n 
  | n > 1     = paso1 (b*mayorDivisorPrimo (b + 1)) (n-1)
  | otherwise = error "Solo admite n positivos"

-- funcion que me encuentre el n-simo primo- numero 1 
productoDePn :: Integer -> Integer
productoDePn n = paso1  1 n

pn :: Integer -> Integer
pn 1 = 2
pn n = mayorDivisorPrimo ( productoDePn (n-1) + 1 )
--------------------------------------------------------------------------
-- Mati

-- La función posición te dice que numero esta en la posición n de la lista
-- Ejemplo: en la posicion 4 de [1,2,3,22,5] esta el 22
-- Entonces posicion 4 [1,2,3,22,5] -> 22
posicion :: Int -> [Int] -> Int
posicion n l = head (nVecesTail (n-1) l)

nVecesTail :: Int -> [Int] -> [Int]
nVecesTail 0 l = l
nVecesTail n l 
  | n == 1    = tail l
  | otherwise =tail (nVecesTail (n-1) l)

longitudHasta :: Int -> [Int] -> Int
longitudHasta _ [] = 0
longitudHasta n (x:xs)
  | n /= x    = 1 + longitudHasta n xs
  | otherwise = 1

hastaunoantes :: Int -> [Int] -> [Int]
hastaunoantes i l = reverse (nVecesTail ((length l) - (longitudHasta (posicion (longitudHasta (i-1) l) l) l)) (reverse l))

cambiarxeni :: Int -> Int -> [Int] -> [Int]
cambiarxeni x i l
  | x /= posicion i l = hastaunoantes i l ++ [x] ++ nVecesTail i l
  | otherwise   = l


enesimo :: Integer -> [a] -> a
enesimo n []       = error "enésimo con n mayor que la longitud de la lista!"
enesimo n (x : xs) 
  | n == 1         = x
  | otherwise      = enesimo (n - 1) xs



funcion :: Double -> Double
funcion x = (18.75)*(1-(2.71828182845905)^((-0.91)*((x-(-0.01)))




