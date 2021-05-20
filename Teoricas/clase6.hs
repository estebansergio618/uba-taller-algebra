sumatoria :: [Int] -> Int
-- que indica la suma de los elementos de una lista
-- sumatoria [] = 0
-- sumatoria l = head l + sumatoria (tail l)
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

longitud :: [Int] -> Int
longitud [] = 0
longitud l = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x : xs ) = n == x || pertenece n xs
-- pertenece x l | l == [] = False
--               | otherwise = x == head l || pertenece x (tail l)

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiploDe45345 (tail l)

-------------------------------------------------------------------------------
{-
        Int, Integer, Float, Double, ...
        (Int,Int), (Int, Float, Float), 
        (Int, (Float, Float), Integer)

        x :: (Int,Int,Int)    y :: (Int,Int,Int,Int)

        Upla = secuencia ordenada finita de una longitud fija 
               de valores de tipos posiblemente distintos
-}

{-
Lista = secuencia ordenada de cosas de longitud variable
        de valores tipos homogéneos
-}

{-
Armar listas:

   Literales:    [1, 2, 6, 3, 5] :: [Int]
                 [1.5, 0.2] :: [Float]
                 [[1], [], [3, 4, 5]] :: [[Int]]
                 [(1,2), (3,4)] :: [(Int, Int)]
                 [1 .. 10] :: [Int]
                 [2, 2, .. 20]
                 [10 .. 1]
                 [10, 9 .. 1]
                 [1..]

   Construcción con el operador :   (cons)

                (:)  :  a -> [a] -> [a]

                1 : [2,3,4]   -----> [1,2,3,4]

   Construcción con []   (nil)

                []


Algunas funciones

        length :: [a] -> Int
        null : [a] -> Bool

                null [1,2,3,4] ---> False
                null []        ---> True

        (++) :: [a] -> [a] -> [a]

                [1,2,3] ++ [4,5,6] ----> [1,2,3,4,5,6]

        reverse :: [a] -> [a]

                reverse [1,2,3,4]  --> [4,3,2,1]


        etc, etc, etc...
-}

{-
        repetir 3 True ---> [True, True, True]
        repetir 5 1.0  ---> [1.0, 1.0, 1.0, 1.0, 1.0]
        repetir 0 'a'  ---> [] 
        repetir (-3) 7 ---> error

        repetir n x = [x, x, ......, x] = [x, x, ........... x]
                      *****************    *  ****************
                         n copias          1       n-1 copias

                    = [x] ++ [x, ..... x]
                      ***    ************
                       1       n-1 copias

                    = [x] ++ repetir (n-1) x

-}
repetir :: Int -> Int -> [Int]
repetir 0 _ = []
repetir n x = [x] ++ repetir (n-1) x

{-
        segmento 1 5   ---> [1,2,3,4,5]
        segmento 3 7   ---> [3,4,5,6,7]
        segmento 10 10 ---> [10]
        segmento 9 4   ---> []

        segmento a b | a == b    = [a]
        segmento a b | a > b     = []

        a <= b:

        segmento a b = [a, a+1, a+2, ..., b]
                        *  ****************

                     = [a] ++ segmento (a + 1) b

        segmento 4 4 
        [4] ++ segmento (4+1) 4
        [4] ++ segmento 5 4 
        [4] ++ []
        [4]
-}
segmento :: Int -> Int -> [Int]
segmento a b
  | a > b       = []
  | otherwise   = a : segmento (a + 1) b

{-
        ciclo 3 [1,2,3] ---> [1,2,3,1,2,3,1,2,3]
        ciclo 4 [0,1]   ---> [0,1,0,1,0,1,0,1]
        ciclo 4 [2]     ---> [2,2,2,2]
        ciclo 4 []      ---> []
        ciclo 2 [[1,2]] ---> [[1,2], [1,2]]
-}

ciclo :: Int -> [a] -> [a]
ciclo 0 xs = []
ciclo n xs = xs ++ ciclo (n-1) xs

{-

        f 4 [1,2,3]  ---> [4,4,4]

        f n xs = [n,n,n,n,n]
                 ***********
                   length xs
-}

f :: Int -> [a] -> [Int]
f n xs = repetir (length xs) n

{-
      g a b = [b, b-1, ... a] = [b] ++ [b-1, ..., a] = [b] ++ g a (b-1)
               *  **********
-}

g :: Int -> Int -> [Int]
g a b 
  | b < a     = []
  | otherwise = b : g a (b - 1)


{-
h a b c = [b, b-c, b-2c, b-3c, .... ]

        h 1 10 2 = [10, 8, 6, 4, 2]
        h 3 9 3  = [9, 6, 3]
        h 4 15 5 = [15, 10, 5]


        h a b c = [b, b-c, b-2*c, .... ]
                  *** ******************
                = [b] ++ [b-c, b-c-c, b-c-2*c, .... ]
                = [b] ++ h a (b-c) c


        h a b c = []

-}

h :: Int -> Int -> Int -> [Int]
h a b c
  | b < a      = []
  | otherwise = b : h a (b - c) c

--  observemos que g a b = h a b 1

{-
   construir una "matriz" de unos de n por n:

   matriz 4 --> [[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]]

-}
-- matriz :: Int -> [Int]
-- matriz n = repetir n (repetir n 1)

{-
 
  triangulo 4 ---> [[1], [1,1], [1,1,1], [1,1,1,1]]
  triangulo 3 ---> [[1], [1,1], [1,1,1]]
  triangulo 1 ---> [[1]]

  triangulo n = [ [1], [1,1], [1,1,1], ...., [1,1,.. n copias... 1] ]
        = [ [1], [1,1], [1,1,1], ...., [1,1,.. (n-1) copias... 1] ]
          ++ [[1,1,.. n copias... 1] ]
        = triangulo (n-1) ++ [repetir n 1]

-}

triangulo :: Int -> [[Int]]
triangulo 0 = []
triangulo n = triangulo (n - 1) ++ [repetir n 1]

{-
 
cuadrados n = lista de los primeros n cuadrados = [1^2, 2^2, 3^2, ... n^2]

cuadrados 3 = [1, 4, 9]
cuadrados 1 = [1]
cuadrados 0 = []

        cuadrados n = [1^2, 2^2, 3^3, ..., (n-1)^2, n^2]
                    = [1^2, 2^2, 3^3, ..., (n-1)^2] ++ [n^2]
                    = cuadrados (n-1) ++ [n^2]


-}

cuadrados :: Int -> [Int]
cuadrados 0 = []
cuadrados n = cuadrados (n - 1) ++ [n ^ 2]

divisores :: Integer -> [Integer]
divisores n = divisoresDesde 1 n

-- divisoresDesde 5 12 = [6, 12]
-- divisoresDesde 3 7 = [7]
divisoresDesde :: Integer -> Integer -> [Integer]
divisoresDesde a n 
  | n < a          = []
  | n`mod`a == 0   = a : divisoresDesde (a + 1) n
  | otherwise      = divisoresDesde (a + 1) n

esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2 

primosHasta :: Integer -> [Integer]
primosHasta n = primosDesdeHasta 1 n 

primosDesdeHasta :: Integer -> Integer -> [Integer]
primosDesdeHasta m n 
  | n < m               = []
  | esPrimo m           = m : primosDesdeHasta (m+1) n
  | otherwise           = primosDesdeHasta (m+1) n


