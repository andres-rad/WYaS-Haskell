import Prelude hiding (curry, uncurry)

--Ej 2
--Definir la funcion curry, que dada una funcion de dos argumentos, devuelve su equivalente currificada.
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x,y)
-- Definir la funcion uncurry, que dada una funcion currificada de dos argumentos, devuelve su version no currificada equivalente.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x,y) = f x y


--Ej 4
--Dar una definicion util de una tripla pitagorica (una tripla (a,b,c) tal que a^2 + b^2 = c^2)
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, n - a, a^2 + (n - a)^2 ) | n <- [1..], a <- [1..n-1]]

--Ej 5
--Generar una lista de los primeros mil numeros primos
milPrimos :: [Integer]
milPrimos = take 1000 [n | n <- [2..], esPrimo n]
        where esPrimo n = [m | m <- [2..n], mod n m == 0] == [n]

--Ej 6
--Escribir la funcion partir que, dada una lista xs devuelve todas las maneras posibles
--de partirla en dos sublistas xs1 xs2 tal que xs == xs1 ++ xs2
partir :: [a] -> [([a], [a])]
partir xs = [(take n xs, drop n xs) | n <- [0..length xs]]

--Ej 7
--Escribir la funcion listasQueSuman que dad un numerp natural n, devuelve todas las listas
--de enteros positivos cuya suma sea n
listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = concat $ [map (m :) $ listasQueSuman (n-m) | m <- [1..n]]

--Ej 8
--Definir en Haskell una lista que contenga todas las listas finitas de enteros positivos
todasLasListas :: [[Int]]
todasLasListas = undefined

--Ej 9
--La tecnica de Divide & Conquer consiste en dividir un problema en problemas mas faciles de resolver
--y luego combinando los resultados parciales, lograr obtener un resultado general

type DivideConquer a b = (a -> Bool)    --determina si es o no el caso trivial
                      -> (a -> b)       --resuelve el caso trivial
                      -> (a -> [a])     --parte el problema en sub-problemas
                      -> ([b] -> b)     --combina resultados
                      -> a              --estructura de entrada
                      -> b              --resultado

--Definir las siguintes funciones:
--dc que implementa la tecnica
dc :: DivideConquer a b
dc trivial solve split combine x = 
    case trivial x of
        True -> solve x
        False -> combine $ map (dc trivial solve split combine) $ split x

--mergeSort en terminos de dc
mergeSort :: Ord a => [a] -> [a]
mergeSort = dc trivial solve split combine
    where
        trivial = (<=1).length
        solve = id
        split xs = map ($xs) [take $ div (length xs + 1) 2, drop $ div (length xs + 1) 2]
        combine = merge

merge [xs,[]] = xs
merge [[], ys] = ys
merge [xs, ys] = menores ++ merge [ys, mayores]
    where (menores, mayores) = break (>=head ys) xs

--Utilizar el esquema dc para reimplementar map y filter
mapDC :: (a -> b) -> [a] -> [b]
mapDC f = dc trivial solve split combine 
    where
        trivial = (<=1).length
        solve xs = if null xs then [] else [f $ head xs]
        split xs = [[x] | x <- xs]
        combine = concat

filterDC :: (a -> Bool) -> [a] -> [a]
filterDC p = dc trivial solve split combine
    where
        trivial = (<=1).length
        solve xs = case null xs of
                        True -> []
                        False -> if p $ head xs then xs else []
        split xs = [[x] | x <- xs]
        combine = concat

--Ej 10
--Redefinir usando foldr las funciones sum, elem, (++), filter y map
sumF :: [Int] -> Int
sumF = foldr (+) 0

elemF :: Eq a => a -> [a] -> Bool
elemF e = foldr ((||).(==e)) False

concatF :: [a] -> [a] -> [a]
concatF xs ys = foldr (:) ys $ xs

filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr ((++).(\x -> if p x then [x] else [])) []

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr ((:).f) []

--Definir la funcion mejor segun que devuelve el maximo elemento de la lista segun
--una funcion de comparacion utilzando foldr1
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun cmp = foldr1 (\x y -> if cmp x y then x else y)

--Definir la funcion sumaAlt que realiza la suma alternada de los elementos de una lista
--Es decir, da como resultado el primer elemento, menos el segundo, mas el tercero, menos el cuarto...
sumaAlt :: [Int] -> Int
sumaAlt xs = fst $ foldr (\x (val, sum) -> if sum then (val + x, False) else (val - x, True)) (0, mod (length xs) 2 == 1) xs

--Hacer lo mismo que en el punto anterior, pero en sentido inverso
sumaAltRev :: [Int] -> Int
sumaAltRev = sumaAlt . reverse

--Definir la funcion permutaciones que dada una lista devuelve todas sus permiutaciones
--permutaciones :: [Int] -> [[Int]]
--permutaciones = foldr funcion [[]]
funcion :: Int -> [[Int]] -> [[Int]]
funcion = (\x pxs -> concatMap (\pxss -> [take n pxs ++ x : drop n pxs | pxs <- pxss, n <- [0..length pxss - 1]]) pxs)