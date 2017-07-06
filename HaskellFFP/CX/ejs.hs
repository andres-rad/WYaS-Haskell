stops = "pbtdkg"
vowels = "aeiou"

--Genera todas las formas posibles de entrelazar 3 elementos de dos strings

stopVWLstop :: String -> String -> [(Char, Char, Char)]
stopVWLstop xs ys = [(x,y,z) | x <- xs, y <-ys, z<-xs]

-----------------------
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' = foldr (\a b -> if a == True then a else b) False

-----------------------
myAny :: (a -> Bool) -> [a] -> Bool
myAny f  = foldr ((||) . f) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' = \f -> foldr ((||) . f) False

-----------------------
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (==a)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = \x -> foldr ((||) . (==x)) False

-----------------------
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' = \f -> foldr ((:) . f) []

-----------------------
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ((++) . \x -> if f x then [x] else []) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' = \f -> foldr ((++) . \x -> if f x then [x] else []) []

-----------------------
squish :: [[a]] -> [a]
squish = foldr (++) []

-----------------------
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = \f -> foldr  ((++) . f) []

-----------------------
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

-----------------------
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == LT then y else x) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == GT then y else x) (head xs) xs


--foldr :: (a -> b -> b) -> b -> [a] -> b