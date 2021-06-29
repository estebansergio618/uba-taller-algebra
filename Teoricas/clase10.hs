ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m)
    | mod b d /= 0 = error "No tiene solución"
    | otherwise    =  (div a d, div b d, div m d)
    where d = gcd a m

solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
    where (d,s,t) = mcdExt a m

-- Función que calcula los enteros del mcd(a,b)= a*x + b*y
mcdExt :: Int -> Int -> (Int, Int, Int)
mcdExt a b | b > a = mcdExt b a 
mcdExt a 0 = (a,0,1)
mcdExt a b = (d,s-t*k,t)
            where (r,k) = ( mod a b,div a b)
                  (d,t,s) = mcdExt b r 