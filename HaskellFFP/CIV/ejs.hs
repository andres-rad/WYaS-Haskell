data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

---------------------------------------
f' :: Int -> String
f' = undefined

g' :: String -> Char
g' = undefined

h' :: Int -> Char
h' = g' . f' 

----------------------------------------
data X
data Y
data Z

xz :: X -> Z 
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (,) (xz x) (yz y)

----------------------------------------
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g a = fst . g . f $ a 