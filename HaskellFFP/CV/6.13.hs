{-

add :: a -> a -> a
add x y = x + y

MAL tipo a demasiado general 

-}

add :: Num a => a -> a -> a
add x y = x + y


{-

addWeird :: Num a => a -> a -> a
addWeird x y =
    if x > 1
    then x + y
    else x

MAL: no se puede deducir Ord

-}

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
    if x > 1
    then x + y
    else x

-------------------------------
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a