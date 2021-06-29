{-# LANGUAGE ImplicitParams #-}
import Data.Char
import qualified Data.Map as Map
import Data.List
import Data.Either
import Data.Functor
import Control.Exception

type Diccionario = Map.Map String Instruccion
type Programa    = [Instruccion]
type Operador    = Integer -> Integer -> Integer
data Instruccion = Numero   Integer
                 | Fun      String
                 | Operador Operador
                 | Grupo    [Instruccion]
                 | Cmd      String

-- ######################################################

operadores :: [(String, Operador)]
operadores = reverse . sortOn fst $ [
             ("+",  (+)), ("-",  (-)), ("*",  (*)), ("/",  div), ("%",  mod),
             ("==", convertir (==)), ("<",  convertir (<)),  (">",  convertir (>))
             ]
  where convertir f a b = toInteger $ fromEnum $ f a b

rpn :: (?dic :: Diccionario) => Programa -> [Integer] -> IO (Diccionario, [Integer]) 
rpn []                           p           = return (?dic, p)
rpn (Grupo g               : qs) p           = rpn (g ++ qs) p
rpn (Numero n              : qs) p           = rpn qs (n : p) 
rpn (Operador f            : qs) (a : b : p) = rpn qs (f a b : p)
rpn (Fun n                 : qs) p 
  | Just q <- Map.lookup n ?dic              = rpn (q : qs) p 
  | otherwise                                = putStrLn ("«" ++ n ++ "» no está definido") >> return (?dic, p)
rpn (Cmd "clear"           : qs) p           = rpn qs [] 
rpn (Cmd "printq"          : qs) p           = putStr (show (reverse p)) >> rpn qs p
rpn (Cmd "print"           : qs) (a : p)     = putStr (show a ++ " ") >> rpn qs (a : p)
rpn (Cmd "ln"              : qs) p           = putStr "\n" >> rpn qs p
rpn (Cmd "def" : Fun n : q : qs) p           = let ?dic = Map.insert n q ?dic in rpn qs p 
rpn (Cmd "if" : si : no    : qs) (a : p)     = rpn ((if a /= 0 then si else no) : qs) p
rpn (Cmd "pop"             : qs) (a : p)     = rpn qs (drop (fromIntegral a) p)
rpn (Cmd "get"             : qs) (a : p)     = rpn qs (p !! fromIntegral a : p) 
rpn (Cmd "put"             : qs) (a : b : p) = rpn qs (reemplazar b a p) 
rpn (Cmd c                 : qs) p           = putStrLn ("«:" ++ c ++ "» no está definido") >> return (?dic, p)
rpn prog                         p           = error "ups!"

reemplazar :: a -> Integer -> [a] -> [a]
reemplazar b 0 (x : xs) = b : xs
reemplazar b n (x : xs) = x : reemplazar b (n - 1) xs

-- ######################################################

leer :: Int -> String -> Either String (Programa, String)
leer d []             
  | d == 0            = return ([], [])
  | otherwise         = Left "se acabó la entrada con grupos sin terminar"
leer d ('#':cs)       = leer d (dropWhile (/= '\n') cs)
leer d ('{':cs)       = leer (d + 1) cs >>= \(prog, cs') -> leer d cs' <&> agregar (Grupo prog)
leer d ('}':cs)       
  | d == 0            = Left "me encontré con un } inesperado"
  | otherwise         = return ([], cs)
leer d (c:cs)
  | isSpace c         = leer d cs
  | isAlpha c         = let (f, cs') = span isName (c:cs)  in leer d cs' <&> agregar (Fun f)
  | c == ':'          = let (f, cs') = span isAlpha cs     in leer d cs' <&> agregar (Cmd f)
  | isDigit c         = let (i, cs') = span isDigit (c:cs) in leer d cs' <&> agregar (Numero (read i))
  | not (null ops)    = leer d (drop (length op - 1) cs) <&> agregar (Operador opf)
  | otherwise         = Left $ "No sé que es «" ++ c:cs ++ "»"
  where ops@(~((op, opf):_)) = [(op, opf) | (op, opf) <- operadores, op `isPrefixOf` (c:cs)]
        isName c      = isAlpha c || c `elem` "_'@"

agregar :: s -> ([s], t) -> ([s], t)
agregar x (a, b) = (x : a, b)

-- ######################################################

main = (try (readFile "preludio") :: IO (Either SomeException String)) <&> fromRight "" >>= evaluar (Map.empty, []) >>= loop
  where loop (dic, p) = print p >> putStr "? " >> getLine >>= evaluar (dic, p) >>= loop
        evaluar (dic, p) linea = case leer 0 linea of
                                   Left msg         -> putStrLn ("!! " ++ msg) >> return (dic, p)
                                   Right (prog, "") -> let ?dic = dic in rpn prog p 
               

gauss :: Integer ->  Integer
gauss 0 = 0
gauss n = n + gauss (n-1)






promedioPonderadoDeTodo :: Double -> Double
promedioPonderadoDeTodo x = ((15 + x)/2)*(0.05) + ((18 + x + 20 + 20 + x)/5)*(0.1) + ((18 + (x+1))/2)*(0.25) + (((x-1)+19)/2)*(0.6)

cuandoSatisface :: Double -> Double -> Double
cuandoSatisface n 0 = error "nada"
cuandoSatisface n x 
  | promedioPonderadoDeTodo x == n = x
  | otherwise                      = cuandoSatisface n (x-1)























