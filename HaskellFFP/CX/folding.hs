{--
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc xs =
  case xs of
    [] -> acc
    (x:xs) -> f x (foldr f acc xs)

foldr (+) 0 [1,2,3]

foldr (+) 0 [1,2,3] =
  case [1,2,3] of
    ...

foldr (+) 0 [1,2,3] = (+) 1 (foldr (+) 0 [2,3])
                    = (+) 1 ((+) 2 (foldr (+) 0 [3]))
                    = (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
                    = (+) 1 ((+2) ((+) 3 (0)))
                    = 1 + (2 + (3 + 0))
                    = 1 + (2 + 3)
                    = 1 + 5
                    = 6


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x ) xs

foldl f acc xs = foldr () acc xs

--------------

foldr (\ a b -> take 3 a) "" ["Pizza", "Apple", "Banana"]


Definición de foldr en función de foldl (solo funciona en listas finitas):

                foldr f z xs = foldl (flip f) z (reverse xs)

--}

pab = foldr (\ a b -> take 3 a ++ b) "" ["Pizza", "Apple", "Banana"]
bap = foldl (\ a b -> take 3 b ++ a) "" ["Pizza", "Apple", "Banana"]

