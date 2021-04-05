--Sergio Esteban - Universidad de Buenos Aires
--Ejercicios de la clase 2

-- 1) estanRelacionados: dados 2 números reales, decide si están relacionados considerando la relacion de equivalencia en R cuyas clases de equivalencia son:

{-
Idea: Sabemos que la igualdad siempre es una relación de equivalencia en los reales(|R) y podemos percatarnos que xRy <-> funcionTecho(x) = funcionTecho(y) y sus clases de equivalencia son los intevalos (n-1,n] donde n es un numero entero. Así 3.56 y 3.84 son equivalentes pues tienen la misma funcionTecho=4.

Haskell tiene la funcionTecho como "ceiling".
-}

estanRelacionados :: (RealFrac a1, RealFrac a2) => a1 -> a2 -> Bool 
-- (Real a, Fractional a) => RealFrac a quiere decir que sus clases de tipo puede ser Real y un Fractional
estanRelacionados x y | ceiling x <= 3 && ceiling y > 3 = False
                      | ceiling x <= 7 && ceiling y > 7 = False
                      | otherwise = True 



-- 2) prodInt: calcula el producto interno entre dos vectores de |R x |R
prodInt :: (Float , Float) -> (Float, Float) -> Float
prodInt v w = (fst v)*(fst w) + (snd v)*(snd w)



-- 3) todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector.
todoMenor ::  (Float , Float) -> (Float, Float) -> Bool
todoMenor v w | (fst v < fst w ) && (snd v < fst w) = True
               | otherwise = False



-- 4) distanciaPuntos: calcula la distancia entre dos puntos de R2.
distanciaPuntos :: (Float , Float) -> (Float, Float) -> Float
distanciaPuntos v w = sqrt ((fst v - fst w)^2 + (snd v - snd w)^2)

-- 5) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x,y,z) = x + y + z

-- 6) posicPrimerPar: dada una terna de enteros, devuelve la posici´on del primer n´umero par si es que hay alguno, y devuelve 4 si son todos impares.
posicPrimerPar :: (Int , Int , Int) -> Int
posicPrimerPar (x,y,z) | mod x 2 == 0 = x
                       | mod y 2 == 0 = y
                       | mod z 2 == 0 = z
                       | otherwise = 4



-- 7) crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por separado (debe funcionar para elementos de cualquier tipo).
crearPar :: p1 -> p2 -> ( p1 , p2)
crearPar x y = ( x , y )




-- 8) invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par´ametro (debe funcionar para elementos de cualquier tipo).
invertir :: ( p1 ,  p2 ) -> ( p2 , p1)
invertir (x,y) = (y,x)