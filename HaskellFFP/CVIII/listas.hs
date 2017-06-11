eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2 
    | o1 > o2 = []
    | o1 == o2 = [o1]
    | otherwise = o1 : eftOrd (succ o1) o2

eftInt :: Int -> Int -> [Int]
eftInt n m
    | n > m = []
    | n == m = [n]
    | otherwise = n : eftInt (succ n) m

eftChar :: Char -> Char -> [Char]
eftChar n m
    | n > m = []
    | n == m = [n]
    | otherwise = n : eftChar (succ n) m

-------------------------------------
--Versión General:

eftGeneral :: (Ord a, Enum a) => a -> a -> [a]
eftGeneral n m
    | n > m = []
    | n == m = [n]
    | otherwise = n : eftGeneral (succ n) m

--------------------------------------
--Ignora espacios al final de la oracion
sepPalabras :: String -> [String]
sepPalabras xs = 
    case dropWhile (/=' ') xs of
        "" -> (case null xs of
            True -> []
            False -> [xs])
        (space:nword) -> (takeWhile (/=' ') xs) : sepPalabras nword


--Si termina en espacio agrega oracion nula al final
sepPalabras' :: String -> [String]
sepPalabras' xs = 
    case dropWhile (/=' ') xs of
        "" -> [xs]
        (space:nword) -> (takeWhile (/=' ') xs) : sepPalabras' nword



--Ignora \n al final del párrafo
listarOraciones :: String -> [String]
listarOraciones xs = 
    case dropWhile (/='\n') xs of
        "" -> (case null xs of
            True -> []
            False -> [xs])
        (nl:s) -> (takeWhile (/='\n') xs) : listarOraciones s


----------------------------------
--Versión de separador general que ignora separadores al final
separarPor :: (Eq a) => a -> [a] -> [[a]]
separarPor sep xs =
    case dropWhile op xs of
        [] -> (case null xs of
                True -> []
                False -> [xs])
        (sp:rest) -> (takeWhile op xs) : separarPor sep rest
    where op = (/=sep)


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

main :: IO ()
main = print $ "Are they equal? " ++ show ((separarPor '\n' sentences) == shouldEqual)


----------------------
--Comprehension de listas

generarAcronimo :: String -> String
generarAcronimo xs = [x | x <- xs, elem x ['A'..'Z']]

-------------------------
--Ordena tres secciones de una lista (nros < 50, entre 50 y 100, >100)
tresSecc :: [Int] -> [Int] 
tresSecc [] = []
tresSecc (x:xs) = bucket1 ++ bucket2 ++ bucket3 
                where
                    next = tresSecc xs
                    bucket1 = case x < 50 of
                        True -> x : next
                        False -> []
                    bucket2 = case x > 100 of
                        True -> x : next
                        False -> []
                    bucket3 = case x >= 50 && x <= 100 of
                        True -> x : next
                        False -> []

             

