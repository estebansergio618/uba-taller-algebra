-- Algoritmos sobre enteros I
digitos :: Integer -> Integer -> [Integer]
digitos 0 _ = []
digitos n b = (n `mod` b) : digitos (n `div` b) b

-- Si quiero su representación como escribiria en matemática
-- deberia darle vuelta a digitos
-- reverse (digitos n b)


numero :: [Integer] -> Integer -> Integer
numero [] _     = 0
numero (x:xs) b = x*b^(enesimo x (x:xs)) + b*(numero xs b) 

enesimo :: Integer -> [a] -> a
enesimo n []       = error "enésimo con n mayor que la longitud de la lista!"
enesimo 0 (x : xs) = x
enesimo n (x : xs) = enesimo (n - 1) xs


