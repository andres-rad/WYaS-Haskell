applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

------------------------------
incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

------------------------------
f :: Bool -> Int
f True = error "blah"
f False = 0

------------------------------
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0 
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n-d) d (count + 1)

------------------------------
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

------------------------------
sumatoria :: (Eq a, Num a) => a -> a
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

------------------------------
multiplicar :: (Integral a) => a -> a -> a
multiplicar 0 n = 0
multiplicar m n = n + multiplicar (m-1) n

-------------------------------
data DividedResult =
     Result Integer |
     DividedByZero

--HACER