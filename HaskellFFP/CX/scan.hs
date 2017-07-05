{--
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
	q : (case ls of
			[] -> []
			x:xs -> scanl f (f q x) xs)
--}

--fibs es una lista infinita con los números de fibonacci
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
fibs20 = fibsN 20
fibsLT100 = takeWhile (<100) fibs

--lista infinita con factoriales
factorial = scanl (*) 1 [1..]
