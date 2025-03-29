

longitud :: [a] -> Int
longitud [] = 0
longitud(_:resto) = 1 + longitud(resto)


sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista(x:resto)= x + sumaLista(resto)



agregaElemento :: [a] -> a -> Bool ->[a]
agregaElemento lista elem True = elem:lista
agregaElemento lista elem False = lista++[elem]


maximoLista :: (Ord a, Num a) => [a] -> a
maximoLista [] = error"No hay un elemento maximo en una lista vacía."
maximoLista [x] = x
maximoLista (x:y:resto) = maximoLista((if x>y
                                        then x
                                        else y):resto)



recuperarElemento :: [a] -> Int -> a
recuperarElemento lista x = if (x<0 || x > (longitud lista)-1)
                        then error"Índice no válido."
                        else lista!!x
                         

divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], n `mod` x==0]

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista lista = [x | (x,i) <- zip lista [0..], x `notElem` take i lista]

soloPares :: [Int] -> [Int]
soloPares lista = [x | x <- lista, even x]






