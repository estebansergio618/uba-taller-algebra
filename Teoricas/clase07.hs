-- ¿CÓmo hacemos una función que calcule el conjunto de partes?
--El orden de los elementos es relevante para las listas, pero no para conjuntos.
--las listas pueden tener elementos repetidos, pero no tiene sentido en conjuntos.

-- Conjunto vacio ========= []
-- {x} U A        ========= (:)/ OJO: 1:[1]--[1,1]
-- x e A          ========= pertenece

-- Ejercicios:
type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y    = True 
    | otherwise = pertenece x ys

agregar :: Int -> Set Int -> Set Int
agregar x c
    | pertenece x c = c
    | otherwise = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] _     = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

cardinal :: Set Int -> Int
cardinal c = length c

-- Tarea: Implementar union, intersección, diferencia y diferencia simetrica.

union :: Set Int -> Set Int -> Set Int
union [] c          = c
union (x:xs) c 
    | pertenece x c = union xs c
    | otherwise     = x : union xs c

interseccion :: Set Int -> Set Int -> Set Int
interseccion _ []          = []
interseccion [] _          = []
interseccion (x:xs) (y:ys) 
    | pertenece x (y:ys)   = x : (interseccion xs (y:ys))
    | pertenece y (x:xs)   = y : (interseccion (x:xs) ys)
    | otherwise            = interseccion xs ys


diferencia :: Set Int -> Set Int -> Set Int
diferencia [] _          = [] 
diferencia _ []          = [] 
diferencia (x:xs) (y:ys) 
    | pertenece x (y:ys) = diferencia xs (y:ys)
    | otherwise          = x : diferencia xs (y:ys)


diferenciaSimetrica :: Set Int -> Set Int -> Set Int 
diferenciaSimetrica c1 c2 = union (diferencia c1 c2) (diferencia c2 c1)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes []     = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

{-pertenece para conjuntos en conjuntos de conjuntos-}
pertenceC :: Set Int -> Set (Set Int) -> Bool
pertenceC _ []       = False
pertenceC xs (ys:yss) = iguales xs ys || pertenceC xs yss

{-agregar conjunto a conjunto de conjuntos-}
agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | pertenceC xs xss = xss
                | otherwise        = xs:xss

{-union de conjuntos de conjuntos-}
unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c       = c
unionC (xs:xss) c | pertenceC xs c = unionC xss c
                  | otherwise      = xs : (unionC xss c)

{-conjunto de partes de un conjunto con todos los naturales hasta n-}
partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = (partesN (n-1)) ++ (agregarATodos n (partesN (n-1))) 
--con n=3 hace esto:
--[] + [1] -> [] [1] + [2] [1,2] -> [] [1] [2] [1,2] + [3] [1,3] [2,3] [1,2,3] 
-- n = 1   ->       n = 2        ->                 n=3

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano _ []   = []
productoCartesiano [] _   = []
productoCartesiano (x:xs) (y:ys) = (productoCartesianoAux x (y:ys)) ++ (productoCartesiano xs (y:ys)) --prod cartesiano del primer elemento con y, sumo el resto recursivamente
    where
        productoCartesianoAux :: Int -> Set Int -> Set (Int, Int) --hace todas las combinaciones de un elemento con un conjunto
        productoCartesianoAux _ []   = [] 
        productoCartesianoAux x (y:ys) = (x,y) : productoCartesianoAux x ys