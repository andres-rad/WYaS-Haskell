--Dar el tipo

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

{--

dodgy 1 :: (Num a) => a -> a
(flip dodgy) 2 :: (Num a) => a -> a

--}

