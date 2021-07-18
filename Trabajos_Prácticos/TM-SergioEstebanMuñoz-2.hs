---------REPRESENTACION DE JUGADAS Y POSICIONES----------------
type Posicion = [Int]
type Jugada   = (Int,Int)
--------------------EJERCICIO-1--------------------------------
jugar :: Posicion -> Jugada -> Posicion
jugar [] _                         = []
jugar (x:xs) (indice, cantidad)
    | indice == 1 && cantidad == x = xs
    | indice == 1                  = (x-cantidad):xs
    | otherwise                    = x:jugar xs (indice - 1, cantidad) 
--------------------EJERCICIO-2--------------------------------
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas []     = []  
posiblesJugadas (x:xs) = jugadasEn x `concatenar` sumaUnoAPrimeraCoordenada (posiblesJugadas xs)    
----------------FUNCIONES AUXILIARES-2-------------------------
sumaUnoAPrimeraCoordenada :: [Jugada] -> [Jugada]
sumaUnoAPrimeraCoordenada []     = []
sumaUnoAPrimeraCoordenada (x:xs) = [suma x (1,0)] `concatenar` sumaUnoAPrimeraCoordenada xs

suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma v w = ((fst v) + (fst w) , (snd v) + (snd w))

jugadasEn :: Int -> [Jugada]
jugadasEn 1 = [(1,1)]
jugadasEn n =  jugadasEn (n-1) `concatenar` [(1,n)]

concatenar :: [a] -> [a] -> [a]
concatenar []       q = q
concatenar (x : xs) q = x : concatenar xs q

--------------------EJERCICIO-3--------------------------------
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = longitud (listaDeJugadasGanadoras p (posiblesJugadas p)) >= 1 
--------------------EJERCICIO-4--------------------------------
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p   = elRivalRecibe p (posiblesJugadas p)
----------------FUNCIONES AUXILIARES-4-------------------------
elRivalRecibe :: Posicion -> [Jugada] -> Jugada
elRivalRecibe [] [j] = j
elRivalRecibe _ [] = error "El rival va a ganar"
elRivalRecibe p (primeraJugada:restoDeJugadas)
    | esJugadaGanadora p (primeraJugada) = primeraJugada
    | otherwise                          = elRivalRecibe p restoDeJugadas
--------------------EJERCICIO-5--------------------------------
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras p = contarJugadasGanadoras p (posiblesJugadas p)
    
contarJugadasGanadoras :: Posicion -> [Jugada] -> Int
contarJugadasGanadoras p []        = 0
contarJugadasGanadoras p (jugadaUno:restoDeJugadas)
    | esJugadaGanadora p jugadaUno = 1 + contarJugadasGanadoras p restoDeJugadas
    |otherwise                     = contarJugadasGanadoras p restoDeJugadas

esJugadaGanadora :: Posicion -> Jugada -> Bool
esJugadaGanadora p jugada = not (esPosicionGanadora(jugar p jugada))
----------------FUNCIONES AUXILIARES-5-------------------------
longitud :: [Jugada] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

listaDeJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
listaDeJugadasGanadoras _ []    = []
listaDeJugadasGanadoras p (x:xs)
    | esJugadaGanadora p x = x:listaDeJugadasGanadoras p xs
    | otherwise            = listaDeJugadasGanadoras p xs

