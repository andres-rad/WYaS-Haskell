import Data.List

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
            where go :: Num a => a -> [a] -> a
                  go n [] = n
                  go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
                where go :: Num a => a -> [a] -> a
                      go n [] = n
                      go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

----------------------------------
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a) 

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f n = case f n of
                Nothing -> []
                Just (x, y) -> x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x 

----------------------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of 
                Nothing -> Leaf
                Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

--Genera un arbol simetrico completo donde cada hoja tiene el valor de su nivel
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == 0 then Nothing else Just (x-1, n-x, x-1)) n


