-- Sergio Esteban Muñoz
-- 2da Lista

-- ==================================================================================================== 
-- Ejercicio 1:
fibonacci :: Integer -> Integer
fibonacci 0         = 0
fibonacci 1         = 1
fibonacci n | n > 1 = fibonacci (n-1) + fibonacci (n-2)
-- ==================================================================================================== 
-- Ejercicio 2:
lucas :: Integer -> Integer
lucas 0         = 2
lucas 1         = 1
lucas n | n > 1 = lucas (n-1) + lucas (n-2)
-- ==================================================================================================== 
-- Ejercicio 3:
-- sucesión a b c d n es el n-ésimo elemento de la sucesión
sucesión :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
sucesión a b c d 0          = a
sucesión a b c d 1          = b
sucesión a b c d n | n > 1  = c*(sucesión a b c d (n-1)) + d*(sucesión a b c d (n-2))

-- fibonacci estaria definido como:
-- sucesión 0 1 1 1 n donde n es el n-ésimo elemento de la sucesión de fibonacci

-- lucas estaria definido como:
-- sucesión 2 1 1 1 n donde n es el n-ésimo elemento de la sucesión de Lucas
-- ==================================================================================================== 
-- Ejercicio 4:

-- Usando la función fibonacci que escribió en el ejercicio ?? ¿puede calcular el 100-ésimo o
-- 1000-ésimo número de Fibonacci?

-- RPTA : No, esa función calcula las mismas sumas muchas veces y esto es medio tonto, lo que hace
-- que se demore mucho tiempo

-- ¿Puede determinar cuántas veces se hace la suma de dos números para calcular fibonacci 10
-- usando la definición que dio?

cantidadDeSumas :: Integer -> Integer
cantidadDeSumas 2 = 1
cantidadDeSumas 3 = 2
cantidadDeSumas n = cantidadDeSumas (n-1) + cantidadDeSumas (n-2) + 1

-- RPTA: Para calcular fibonacci de 10 se utilizan 88 sumas en la deficion que di 

-- ¿Cómo cree que yo pude calcular f1000?

paso :: Integer -> Integer -> Integer -> Integer
paso a b 0         = a
paso a b 1         = b
paso a b n | n > 1 = paso b (a + b) (n - 1)

fib :: Integer -> Integer
fib n = paso 0 1 n

-- Nota: hace un total de 10 sumas fib 10 
-- ==================================================================================================== 
-- Ejercicio 5: (946)
-- termina en 946 empezando desde 1
terminaEn :: Integer -> Integer -> Integer
terminaEn m n = primerFibQue m n

primerFibQue :: Integer -> Integer -> Integer
primerFibQue m n
    | mod f 1000 == m = f
    | otherwise       = primerFibQue m (n+1)
    where f = fib n

-- NOTA: El primer número de fibonacci que termina en 946 es fib 21 = 10946 
-- ==================================================================================================== 
-- Ejercicio 6:
pascal :: Integer -> Integer -> Integer
pascal 0 0 = 1
pascal 1 0 = 1
pascal 1 1 = 1
pascal n k = pascal (n-1) k + pascal (n-1) (k-1)
-- ==================================================================================================== 
-- Ejercicio 7:
-- Escriba una función dígitoMásFrecuente :: Integer -> Integer tal que cada vez que n es un
-- entero positivo dígitoMásFrecuente n sea el número de {0, . . . , n} que más veces aparece como
-- dígito de n. Si hay «empates», elija el más chico.
-- ==================================================================================================== 
-- Ejercicio 8 :
dígitosDecrecientes :: Integer -> Bool 
dígitosDecrecientes n | n < 10 = True
dígitosDecrecientes n          = (decena n <= unidad n) && dígitosDecrecientes (numeroSinLaUnidad n)

numeroSinLaUnidad :: Integer -> Integer
numeroSinLaUnidad n = div n 10

unidad :: Integer -> Integer
unidad n = mod n 10

decena :: Integer -> Integer
decena n = mod (div n 10) 10
-- ==================================================================================================== 
-- Ejercicio 9 :
dígitosOrdenados :: Integer -> Bool
dígitosOrdenados n = dígitosDecrecientes n || dígitosCrecientes n

dígitosCrecientes :: Integer -> Bool
dígitosCrecientes n | n < 10 = True
dígitosCrecientes n          = (decena n >= unidad n) && dígitosCrecientes (numeroSinLaUnidad n)
-- ===========================================FIN====================================================== 
