-- import Data.Numbers.Primes (primeFactors) 

-- Función que calcula los enteros del mcd(a,b)= a*x + b*y
mcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
mcdExt a b | b > a = mcdExt b a 
mcdExt a 0 = (a,1,0)
mcdExt a b = (d,t,s-t*k)
            where (k,r) = (div a b, mod a b)
                  (d,s,t) = mcdExt b r 

--------------------------------------------------------------------------
-- Olimpiada Brasileña (OIbM-1987). Una secuencia P_n es definida de la 
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

productoDePn :: Integer -> Integer
productoDePn 1 = 2
productoDePn n = p * ( mayorDivisorPrimo( p + 1 ) )
                where p = productoDePn (n-1)

pn :: Integer -> Integer
pn 1 = 2
pn n = mayorDivisorPrimo ( productoDePn (n-1) + 1 )
--------------------------------------------------------------------------

