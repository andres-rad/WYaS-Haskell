# Práctica 1 - Programación Funcional - Soluciones

## Ejercicio 1

### I.

```haskell
max2 :: Ord a => (a, a) -> a
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)

subtract :: Int -> Int -> Int
subtract = flip (-)

predecesor :: Int -> Int
predecesor = substract 1

evaluarEnCero :: (Int -> a) -> a
evaluarEnCero = \f -> f 0

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
```

### II.

```haskell
max2 :: a -> a -> a
max2 x y | x >= y = x
         | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt (x^2 + y^2)
```


## Ejercicio 2

### I.

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
```

### II.
```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
```

### III.

No, ya que funciones de distinta aridad tienen un tipo distinto, y no puedo hacer una función en haskell que actúe de manera diferente dependiendo del tipo de la entrada (las funciones definidas con variables de tipo tienen que funcionar para cualquier entrada).

## Ejercicio 3

```haskell
[ x | x <- [1..3], y <- [x..3], (x + y) `mod` 3 == 0] = [1, 3]
-- se cumple para (x, y) = (1, 2) y (3, 3)
```

## Ejercicio 4

```haskell
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | a <- [1..], b <- [1..], c <- [1..], a^2, + b^2 = c^2]
```

Esta definición no es útil porque solo me sirve para computar las triplas pitagóricas de la pinta (1, 1, X). El problema está en que tengo más de un generador infinito.

Ideas:

* *c* está unívocamente determinado por *a* y *b*.
* Tengo que generar los pares de enteros realizando en todo momento pasos finitos.
* Puedo enumerar los pares en función de la suma de sus componentes: primero los pares que suman 2, después los que suman 3... Para cada paso hay una cantidad finita de pares que cumplen la propiedad, así que voy a poder calcular cualquier par sin que se cuelgue.

```haskell
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(b, a-b, b^2 + (a-b)^2) | a <- [2..], b <- [1..a-1]]
```

## Ejercicio 5

```haskell
take 1000 [x | x <- [3..], [y | y <- [1..x-1], x `mod` y == 0] == [1]]

-- Un poco mas declarativo
take 1000 [x | x <- [3..], esPrimo x]

esPrimo :: Int -> Bool
esPrimo = length divisores x = 1

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], x `mod` y = 0]
```

## Ejercicio 6

```haskell
partir :: [a] -> [([a], [a])]
partir xs = [(take amt xs, drop amt xs) | amt <- [0..length xs]]
```

## Ejercicio 7

```haskell
listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = sinRepetidos . concat $ [concat $ map (insertarEnCadaPosicion x) (listasQueSuman (n-x)) | x <- [1..n]]

insertarEnCadaPosicion :: a -> [a] -> [[a]]
insertarEnCadaPosicion elem lista = [insertarEnIesimaPosicion elem i lista | i <- [0..length lista]]

insertarEnIesimaPosicion :: a -> Int -> [a] -> [a]
insertarEnIesimaPosicion elem i lista = take i lista ++ (elem : (drop i lista))

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos ls = [ ls !! i | i <- [0..length ls-1], not $ (ls !! i) `elem` (take i ls)] 
```

## Ejercicio 8

```haskell
todasLasListas = concat [listasQueSuman i | i <- [1..]]
```

## Ejercicio 9

```haskell
type DivideConquer a b = (a -> Bool)
    -> (a -> b)
    -> (a -> [a])
    -> ([b] -> b)
    -> a
    -> b
```

### I

```haskell
dc :: DivideConquer a b
dc trivial solve split combine x =
    case trivial x of
        True -> solve x
        False -> combine $ map (dc trivial solve split combine) (split x)
```

### II

```haskell
mergeSort :: Ord a => [a] -> [a]
mergeSort = dc trivial solve split combine
    where
        trivial xs = length xs == 1
        solve = id
        split xs = map ($ xs) [take (half xs), drop (half xs)]
        combine [xs, []] = xs
        combine [[], ys] = ys
        combine [xs, ys] = (fst $ (break (head ys <)) xs) ++ (combine [ys, (snd $ break (head ys <) xs)]) 
        half ls = length ls `div` 2
```

### III

Este ejercicio es medio raro, dc usa map en la definición.

```haskell
map2 :: (a -> b) -> [a] -> [b]
map2 f =  dc trivial solve split combine 
    where
        trivial xs = length xs <= 1
        solve xs = if null xs then [] else [f x] 
        split xs = [take (half xs) xs, drop (half xs) xs]
        combine = concat
        half ls = length ls `div` 2

filter :: (a -> Bool) -> [a] -> [a]
filter pred = dc trivial solve split combine
    where
        trivial xs = length xs <= 1
        solve [] = []
        solve [x] = if pred x then [x] else []
        split xs = [take (half xs) xs, drop (half xs) xs]
        combine = concat
        half ls = length ls `div` 2
```

## Ejercicio 10

### I

```haskell
sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 e = foldr (\x rec -> rec || e == x) False

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys $ xs

filter :: (a -> Bool) -> [a] -> [a]
filter pred = foldr (\x rec -> if pred x then (x:rec) else rec) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
```

### II

```haskell
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun cmp = foldr1  (\x y -> if cmp x y then x else y)
```

### III

```haskell
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0
```

### IV

```haskell
sumaAltInv = sumaAlt . reverse
```

### V

```haskell
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x -> concatMap $ insertarEnCadaPosicion x) [[]]

insertarEnCadaPosicion :: a -> [a] -> [[a]]
insertarEnCadaPosicion elem lista = [insertarEnIesimaPosicion elem i lista | i <- [0..length lista]]

insertarEnIesimaPosicion :: a -> Int -> [a] -> [a]
insertarEnIesimaPosicion elem i lista = take i lista ++ (elem : (drop i lista))
```

## Ejercicio 11

### I

```haskell
partes :: [a] -> [[a]]
partes xs = reverse $ concat [sublistasSinIElementos i 1 xs | i <- [0..length xs]]

sublistasSinIElementos :: Int -- Cuantos elementos saco de la lista
                       -> Int -- Indice a partir del cual puedo sacar elementos (para evitar repetidos)
                       -> [a] -- Lista a la cual le saco elementos
                       -> [[a]] -- Todas las listas resultantes de sacar I elementos a partir del indice indicado
sublistasSinIElementos 0 _ xs = [xs]
sublistasSinIElementos n s xs =
    case length xs == n of
        True -> [[]]
        False -> concat [sublistasSinIElementos (n-1) i $ take (i-1) xs ++ drop i xs | i <- [s..length xs]]
```

### II

```haskell
prefijos :: [a] -> [[a]]
prefijos xs = [take i xs | i <- [0..length xs-1]]
```

### III

```haskell
prefijosNoVacios :: [a] -> [[a]]
prefijosNoVacios xs = [take i xs | i <- [1..length xs]]

sufijosNoVacios :: [a] -> [[a]]
sufijosNoVacios xs = [drop i xs | i <- [0..length xs-1]]

sublistas :: [a] -> [[a]]
sublistas xs = [] : (concatMap prefijosNoVacios $ sufijosNoVacios xs)
```

## Ejercicio 12

```haskell
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
```

### a

```haskell
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem = recr (\x xs rec -> if x == elem then xs else x:rec) []
```

### b

Si pensamos a una instancia de un tipo como un árbol, para poder definir una función con foldr hace falta que el resultado recursivo dependa solamente del subárbol del elemento que estoy mirando. En el caso de sacarUna el resultado recursivo depende del comienzo de la lista.

### c

No, listasQueSuman recibe un entero, no se construye haciendo recursión sobre una lista.

## Ejercicio 13

```haskell
genLista :: a -> (a -> a) -> Int -> [a]
genLista base genF n = take n listaInf
    where
        listaInf = base : (map genF listaInf)

desdeHasta :: Int -> Int -> [Int]
desdeHasta desde hasta = genLista desde (+1) (hasta - desde + 1)
```

## Ejercicio 14


### I

```haskell
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares = map . uncurry 
```

### II

```haskell
armarPares :: [a] -> [b] -> [(a, b)]
armarPares xs = foldr fRec (\_ -> []) xs
    where
        fRec :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
        fRec x rec ys = if null ys then [] else (x, head ys) : (rec $ tail ys) 
```

### III

```haskell
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares = map . uncurry 

armarPares :: [a] -> [b] -> [(a, b)]
armarPares xs = foldr fRec (\_ -> []) xs
    where
        fRec :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
        fRec x rec ys = if null ys then [] else (x, head ys) : (rec $ tail ys)


mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f $ armarPares xs ys
```

## Ejercicio 15

### I
```haskell
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = (zipWith . zipWith) (+)
```

### II

```haskell
trasponer :: [[Int]] -> [[Int]]
trasponer xxs = foldr (zipWith (:)) (filaVacia xxs) xxs
    where
        filaVacia :: [[Int]] -> [[Int]]
        filaVacia xxs = [[] | i <- [1..length xxs]]
```

## Ejercicio 16

```haskell
generate ::  ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])
```

### I

```haskell
generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop next'
    where next' xs = if null xs then base else next $ last xs
```

### II

```haskell
factoriales :: Int -> [Int]
factoriales n = generate stop next
    where
        stop l = length l > n
        next l =  if null l then 1 else (last l) * (last l + 1) 
```

### III

```haskell
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN veces f base = generateBase stop base f
    where
        stop xs = length xs - 1 == veces
```

### IV

```haskell
generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs = takeWhile  (xs ++ (iterate next $ tail xs))
```

## Ejercicio 17

### I

```haskell
foldNat :: a -> (a -> a) -> Integer -> a
foldNat base _ 0 = base
foldNat base fSucesor n = fSucesor $ foldNat base fSucesor (n-1)
```

### II

```haskell
potencia :: Integer -> Integer -> Integer
potencia base exponente = foldNat 1 (*base) exponente
```

## Ejercicio 18

```haskell
data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)

foldPolinomio :: b
              -> (a -> b)
              -> (b -> b -> b)
              -> (b -> b -> b)
              -> Polinomio a
              -> b
foldPolinomio fX fCte fSuma fProd poli =
    case poli of
        X -> fX
        Cte a -> fCte a
        Suma p1 p2 -> fSuma (rec p1) (rec p2)
        Prod p1 p2 -> fProd (rec p1) (rec p2)
    where
        rec = foldPolinomio fX fCte fSuma fProd 

evaluar :: Num a => a -> Polinomio a -> a
evaluar val = foldPolinomio fX fCte fSuma fProd
    where
        fX = val
        fCte = id
        fSuma = (+)
        fProd = (*)  
```

## Ejercicio 19

```haskell
type Conj a = (a -> Bool)
```

### I

```haskell
vacio :: Conj a
vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar elem fConj x = x == elem || fConj x
```

### II

```haskell
interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 x = c1 x && c2 x

union :: Conj a -> Conj a -> Conj a
union c1 c2 x = c1 x || c2 x
```

### III

```haskell
puntoFijoEn0 :: Conj (Int -> Int)
puntoFijoEn0 = (== 0) . ($0)
```

### IV

```haskell
singleton :: Eq a => a -> Conj a
singleton = (==)
```

### V

No

```haskell
mapConj :: (a -> b) -> Conj a -> Conj b
mapConj f ConjA b = ??? -- f :: a -> b, ConjA :: a -> Bool, b :: b
```

La función mapeada tiene que recibir elementos de tipo *b*, pero la función de map recibe elementos de tipo *a*, así que no tengo manera de generar algo de tipo *Conj b* a partir de un *b*. Si podría hacerlo si la funcion del map fuese de tipo *(b -> a)*

```haskell
contraMapConj :: (b -> a) -> Conj a -> Conj b
contraMapConj f ConjA b = ConjA $ f b
```

Dado un elemento de *b* lo convierto en un elemento de tipo *a* con la función y me fijo si está en el conjunto original. El tipo de Conj es **contravariante** en su argumento.

## Ejercicio 20

```haskell
type MatrizInfinita a = Int->Int->a
```

### I

```haskell
fila :: Int -> MatrizInfinita a -> [a]
fila f mat = mat f

columna :: Int -> MatrizInfinita a -> [a]
columna c mat = flip mat c
```

### II

```haskell
transponer :: MatrizInfinita a -> MatrizInfinita a
transponer = flip
```

### III

```haskell
mapMatriz :: (a -> b) -> MatrizInfinita a -> MatrizInfinita b
mapMatriz f m i j = f $ m i j

filterMatriz :: (a -> Bool) -> MatrizInfinita a -> [a]
filterMatriz pred m = [m i (n - i) | n <- [0..], i <- [0..n], pred $ m i (n - i)]

zipWithMatriz :: (a -> b -> c) -> MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita c
zipWithMatriz fZip mA mB i j = fZip (mA i j) (mB i j)
```

### IV

```haskell
suma :: Num a => MatrizInfinita a -> MatrizInfinita a -> MatrizInfinita a
suma = zipWithMatriz (+)

zipMatriz :: MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita (a, b)
zipMatriz = zipWithMatriz (,)
```

## Ejercicio 21

```haskell
data AHD tInterno tHoja = Hoja tHoja
                        | Rama tInterno (AHD tInterno tHoja)
                        | Bin (AHD tInterno tHoja) tInterno (AHD tInterno tHoja) deriving Show
```
### I

```haskell
foldAHD :: (tHoja -> tRes)
        -> (tInterno -> tRes -> tRes)
        -> (tRes -> tInterno -> tRes -> tRes)
        -> AHD tInterno tHoja
        -> tRes

foldAHD fHoja fRama fBin ahd =
    case ahd of
        Hoja h -> fHoja h
        Rama nodo hijo -> fRama nodo (rec hijo)
        Bin hIzq nodo hDer -> fBin (rec hIzq) nodo (rec hDer)
    where
        rec = foldAHD fHoja fRama fBin
```

### II

```haskell
mapAHD :: (a -> b) -> (c -> d) -> AHD a c -> AHD b d
mapAHD fInt fH = foldAHD fHoja fRama fBin
    where
        fHoja = Hoja . fH
        fRama int hijo = Rama (fInt int) hijo
        fBin hIzq int hDer = Bin hIzq (fInt int) hDer 
```

## Ejercicio 22

```haskell
data AB a = Nil | Bin (AB a) a (AB a)
```

### I

```haskell
foldAB :: b
       -> (b -> a -> b -> b)
       -> AB a
       -> b
foldAB fNil fBin ab =
    case ab of
        Nil -> fNil
        Bin izq v der -> fBin (rec izq) v (rec der)
    where
        rec = foldAB fNil fBin
```

### II
```haskell


esNil :: AB a -> Bool
esNil ab = 
    case ab of
        Nil -> True
        _ -> False

{--
esNil = foldAB True fBin
    where
        fBin _ _ _= False
--}

altura :: AB a -> Integer
altura = foldAB 0 fBin
    where
        fBin altIzq _ altDer = max altIzq altDer + 1

ramas :: AB a -> Integer
ramas = foldAB 0 fBin
    where
        fBin ramasIzq _ ramasDer = ramasIzq + ramasDer

nodos :: AB a -> Integer
nodos = foldAB 0 fBin
    where
        fBin nodosIzq _ nodosDer = nodosIzq + nodosDer + 1

hojas :: AB a -> Integer
hojas = foldAB 0 fBin
    where
        fBin 0 _ 0 = 1
        fBin hojasIzq _ hojasDer = hojasIzq + hojasDer

espejo :: AB a -> AB a
espejo = foldAB Nil fBin
    where
        fBin izq v der = Bin der v izq
```

### III

```haskell
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB fNil fBin
    where
        fNil = esNil
        fBin mismaEstIzq _ mismaEstDer ab2 =
            case ab2 of
                Nil -> False
                Bin izq _ der -> mismaEstIzq izq && mismaEstDer der
```

## Ejercicio 23

### I

```haskell
data RoseTree a = RT a [RoseTree a]
```

### II

```haskell
foldRT :: (a -> [b] -> b)
       -> RoseTree a
       -> b
foldRT fRT (RT a hijos) = fRT a (map rec hijos)
    where rec = foldRT fRT
```

### III

#### a

```haskell
hojas :: RoseTree a -> [a]
hojas = foldRT fRT
    where
        fRT v [] = [v]
        fRT v xs = concat xs

distancias :: RoseTree a -> [Integer]
distancias = foldRT fRT
    where 
        fRT _ [] = [0]
        fRT a distanciasHijos = map (+1) $ concat distanciasHijos

altura :: RoseTree a -> Integer
altura = foldRT fRT
    where
        fRT _ [] = 0
        fRT a alturasHijos = 1 + (maximum alturasHijos)
```