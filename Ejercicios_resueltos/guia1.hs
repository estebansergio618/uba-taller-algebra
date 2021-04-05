--Sergio Esteban - Universidad de Buenos Aires
--Ejercicios de la clase 2

--Absoluto: Calcular el valor absoluto de un número entero

valorAbsoluto :: Int -> Int
valorAbsoluto n | n >= (-n) = n
                | otherwise = (-n)

--maximoAbsoluto: Devuelve el máximo entre el valor absoluto de 2 números

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto n1 n2 | valorAbsoluto n1 >= valorAbsoluto n2 = valorAbsoluto n1
                     | otherwise = valorAbsoluto n2

--maximo3: Devuelve el máximo entre 3 numeros enteros

maximo3 ::  Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z

--algunoEs0: dados dos números racionales, decide si alguno de los dos es igual a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).

--Sin pattern matching
algunoEs0 :: Float -> Float -> Bool
algunoEs0 r1 r2 | (r1 == 0) || (r2 == 0) = True
                | otherwise = False

--Con pattern matching
{-
algunoEs0 :: Float -> Float -> Bool
algunoEs0 0 r2 = True
algunoEs0 r1 0 = True
algunoEs0 r2 r1 = False
-}

--ambosSon0: dados dos n´umeros racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).

--Sin pattern matching
ambosSon0 :: Float -> Float -> Bool
ambosSon0 r1 r2 | (r1 == 0) && (r2 == 0) = True
                | otherwise = False

--Con pattern matching
{-
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 r1 r2 = False
-}

--esMultiploDe: dados dos números naturales, decidir si el primero es múltiplo del segundo

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n1 n2 | mod n1 n2 == 0 = True
                   | otherwise = False

--digitoUnidades: dado un número natural, extrae su dígito de las unidades.

digitoUnidades :: Int -> Int  
digitoUnidades n = mod (valorAbsoluto n) 10

--digitoDecenas: dado un número natural, extrae su dígito de las decenas

digitoDecenas :: Int -> Int
digitoDecenas n = div (mod (valorAbsoluto n) 100) 10
  
                 

