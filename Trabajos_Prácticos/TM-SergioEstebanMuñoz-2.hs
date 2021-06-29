---------REPRESENTACION DE JUGADAS Y POSICIONES----------------
type Posicion = [Int]
type Jugada   = (Int,Int)
--------------------EJERCICIO-1--------------------------------

jugar :: Posicion -> Jugada -> Posicion
jugar [] _         = []
jugar (x:xs) (indice, cantidad) 
    | length x == indice = ((x-cantidad):xs)
    | otherwise = x:jugar xs (indice - 1 , cantidad) 
   
----------------FUNCIONES AUXILIARES 1-------------------------
-- elNumeroEnPosicion :: Int -> [Int] -> Int
-- elNumeroEnPosicion _ [] = error "No hay nada ahí :)"
-- elNumeroEnPosicion n (x : xs) 
--     | n == 1            = x
--     | otherwise         = elNumeroEnPosicion (n - 1) xs

-- quitar :: Int -> [Int] -> [Int]
-- quitar _ []     = error "La lista está vacía."
-- quitar 1 (_:xs) = xs
-- quitar i (x:xs) = x:quitar (i-1) xs

-- nuevaPosicion :: Posicion -> Jugada -> Posicion
-- nuevaPosicion p j = pone ((elNumeroEnPosicion (fst j) p) - (snd j)) (fst j) p

-- pone :: Int -> Int -> Posicion -> Posicion
-- pone n 1 p = n : tail p
-- pone n i p = head p : pone n (i-1) (tail p)

--------------------EJERCICIO-2--------------------------------
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas []     = []  
posiblesJugadas (x:xs) = jugadasEn x `concatenar` sumaUnoAPrimeraCoordenada (posiblesJugadas xs)

------------------RAZONAMIENTO-2----------------------------------
--  Idea : 

--  1) cuando tengo una Posicion vacia no tendre ninguna jugada que hacer, me devolvera una lista sin jugadas []

--  2) cuando tenga una posicion no vacia:
--    por ejemplo:
--     [1,2,2] ->>>>>>> [(1,1),(2,1),(2,2),(3,1),(3,2)]

--    ¿Qué pasa si supongo que tengo las posibles jugadas de la cola de esa Posición?
    
--     [2,2]   ->>>>>>> [(1,1),(1,2),(2,1),(2,2)]
    
--     pues puedo notar que si le sumo uno a la primera cordenada de cada jugada de la lista de jugadas
--     tengo la cola de las posiblesJugadas [1,2,2] 
            
--             [(1+1,1),(1+1,2),(2+1,1),(2+1,2)]
    
--     i) Necesitare una funcion que me << sume uno a la primera coordenada de la lista de posibles jugadas >>

--     Entonces, ya solo me quedaria ver cuales son << las jugadas en >> la cabeza y concatenar estas 2 ideas

--     ii) Necesitare una funcion que me haga << jugadas en >> la cabeza de la lista.

    
----------------FUNCIONES AUXILIARES-2-------------------------
sumaUnoAPrimeraCoordenada :: [Jugada] -> [Jugada]
sumaUnoAPrimeraCoordenada []     = []
sumaUnoAPrimeraCoordenada (x:xs) = [suma x (1,0)] `concatenar` sumaUnoAPrimeraCoordenada xs

-- Para sumar pares de vectores necesito:
suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma v w = ((fst v) + (fst w) , (snd v) + (snd w))

jugadasEn :: Int -> [Jugada]
jugadasEn 1 = [(1,1)]
jugadasEn n =  jugadasEn (n-1) `concatenar` [(1,n)]

--  jugadasEn 4 -> [(1,1),(1,2),(1,3),(1,4)]
--  jugadasEn 3 -> [(1,1),(1,2),(1,3)]

--  jugadasEn 4 = jugadasEn 3 ++ [(1,4)] 


concatenar :: [a] -> [a] -> [a]
concatenar []       q = q
concatenar (x : xs) q = x : concatenar xs q

--------------------EJERCICIO-3--------------------------------
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p  = proximoTurno p (posiblesJugadas p)
------------------RAZONAMIENTO-3----------------------------------
-- Idea :
--     Apoyandome del enunciado del TP una posicion es ganadora si existe una jugada
--     tal que en el proximo turno es una posicion no ganadora.

--     funciones que necesitaré:
--     proximoTurno: me dan una Posicion y todas sus posibles jugadas y me evalua si alguna
--     de esas jugadas es no ganadora. 

----------------FUNCIONES AUXILIARES-3-------------------------
proximoTurno :: Posicion -> [Jugada] -> Bool
proximoTurno _ []                          = False
proximoTurno p (xs:xss)
    | not (esPosicionGanadora(jugar p xs)) = True
    | otherwise                            = proximoTurno p xss

--------------------EJERCICIO-4--------------------------------
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p   = elRivalRecibe p (posiblesJugadas p)

-- voy a probar todas las jugadas posibles y si encuentro una jugada tal que 
-- el rival recibe una posicion perdedora me devuelca esa jugada, en caso contrario
-- que siga probando.

----------------FUNCIONES AUXILIARES-4-------------------------
elRivalRecibe :: Posicion -> [Jugada] -> Jugada
elRivalRecibe [] [j] = j
elRivalRecibe _ [] = error "El rival va a ganar"
elRivalRecibe p (xs:xss)
    | not (esPosicionGanadora(jugar p xs)) = xs
    | otherwise                            = elRivalRecibe p xss

--------------------EJERCICIO-5--------------------------------
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras p  =  longitud (listaDeJugadasGanadoras p (posiblesJugadas p))
----------------FUNCIONES AUXILIARES-5-------------------------
longitud :: [Jugada] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

listaDeJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
listaDeJugadasGanadoras _ []               = []
listaDeJugadasGanadoras p (x:xs)
    | not (esPosicionGanadora(jugar p x))  = x:listaDeJugadasGanadoras p xs
    | otherwise                            = listaDeJugadasGanadoras p xs



