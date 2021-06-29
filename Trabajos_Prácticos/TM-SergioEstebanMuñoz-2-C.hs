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
{-
MG:
Ejercicio 1: A-. Resuelve pero debería salir "más recursivamente", es decir, hay mucha función auxiliar intentando resolver problemas que de haberlo pensado recursivamente no serían necesarias.
- Las funciones auxiliares no son muy declarativas, complementá con comentarios si es necesario, en general eso ayuda mucho
- Usar pattern matching con la tupla de la Jugada ayudaría a hacer el código más legible.
- Ojo que si m < (snd j) estamos vaciando igual ese montoncito. No está mal pero tampoco estoy seguro que sea el comportamiento deseado.
- Debería poder salir con menos funciones auxiliares. Si pensas la función jugar recursivamente, no es necesario ni poner ni quitar, incluso podría salir todo en una sola función, ¿Se te ocurre cómo?

S1>> Volvi a hacer la funcion jugar de nuevo con las observaciones.

-}
--------------------EJERCICIO-2--------------------------------
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas []     = []  
posiblesJugadas (x:xs) = jugadasEn x `concatenar` sumaUnoAPrimeraCoordenada (posiblesJugadas xs)
----------------FUNCIONES AUXILIARES-2-------------------------
jugadasEn :: Int -> [Jugada]
jugadasEn 1 = [(1,1)]
jugadasEn n = [(1,n)] `concatenar` jugadasEn (n-1)

sumaUnoAPrimeraCoordenada :: [Jugada] -> [Jugada]
sumaUnoAPrimeraCoordenada []     = []
sumaUnoAPrimeraCoordenada (x:xs) = [suma x (1,0)] `concatenar` sumaUnoAPrimeraCoordenada xs

suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma v w = ((fst v) + (fst w) , (snd v) + (snd w))

concatenar :: [a] -> [a] -> [a]
concatenar []       q = q
concatenar (x : xs) q = x : concatenar xs q

--------------------EJERCICIO-3--------------------------------
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p  = proximoTurno p (posiblesJugadas p)
{-
MG:
Ejercicio 3: A
- proximoTurno devuelve un Bool y eso confunde un poco. Yo leo una función con ese nombre y no espero que me diga que eso es verdadero o falso.

S2>> Apoyandome del enunciado del TP una posicion es ganadora si existe una jugada
     tal que en el << proximo turno >> es una posicion no ganadora.

     Entonces necesitaré una función que defino y hace:
     proximoTurno --> me dan una Posicion y todas sus posibles jugadas, me evalue si alguna de esas jugadas me de una posicion no ganadora. 
-}
----------------FUNCIONES AUXILIARES-3-------------------------
proximoTurno :: Posicion -> [Jugada] -> Bool
proximoTurno _ []       = False
proximoTurno p (xs:xss) = esPosicionpPerdedora p xs || proximoTurno p xss

esPosicionpPerdedora :: Posicion -> Jugada -> Bool
esPosicionpPerdedora p xs = not (esPosicionGanadora(jugar p xs))

--------------------EJERCICIO-4--------------------------------
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p   = elRivalRecibe p (posiblesJugadas p)
{-
MG: 
Ejercicio 4: A
- El nombre elRivalRecibe no es muy declarativo, no entiendo qué hace ni por qué eso devuelve una Jugada

S3>>> -- Haré una función que va a probar todas las jugadas posibles en una Posicion y si encuentro una 
    jugada tal que << el rival recibe >> una posicion perdedora me devuelva esa jugada, en caso contrario que siga probando.

- Fijate que en el ejercicio 3, 4 y 5 repetis la expresión not "(esPosicionGanadora(jugar p xs))". Incluso, repetis el esquema recursivo, es decir, lo pones en una guarda y si no entra por ahí en el otherwise haces el llamado recursivo. Todo esto es indicio de que deberías encapsular eso en una función.

S4>> Encapsulé la expresión "not (esPosicionGanadora(jugar p xs))" en una funcion llamada esPosicionPerdedora 
    en FUNCIONES AUXILIARES-3 porque es desde donde empecé a repetirla como usted me dijo.


      --esPosicionpPerdedora :: Posicion -> Jugada -> Bool
      --esPosicionpPerdedora p xs = not (esPosicionGanadora(jugar p xs))

-}
----------------FUNCIONES AUXILIARES-4-------------------------
elRivalRecibe :: Posicion -> [Jugada] -> Jugada
elRivalRecibe [] [j] = j
elRivalRecibe _ []   = (0,0) -- Utilizo (0,0) para representar que el rival va a ganar
elRivalRecibe p (x:xs)
    | esPosicionpPerdedora p x = x
    | otherwise                 = elRivalRecibe p xs

--------------------EJERCICIO-5--------------------------------
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras p  =  longitud (listaDeJugadasGanadoras p (posiblesJugadas p))

{-
MG:
Ejercicio 5: A
- También podría salir sin listar las soluciones y ver la longitud de la lista, ¿Se te ocurre cómo?

S5>> Lo intenté pero no encontre otra forma de resolverlo

-}
----------------FUNCIONES AUXILIARES-5-------------------------
longitud :: [Jugada] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

listaDeJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
listaDeJugadasGanadoras _ []    = []
listaDeJugadasGanadoras p (x:xs)
    | esPosicionpPerdedora p x  = x:listaDeJugadasGanadoras p xs
    | otherwise                 = listaDeJugadasGanadoras p xs


