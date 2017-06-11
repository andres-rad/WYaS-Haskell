{--

putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()


print a = putStrLn . show $ a

--}

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = (mod (div x 10) 10)

hunsD x = tensDigit (x `div` 10)

----------------------------------
foldBool :: a -> a -> Bool -> a
foldBool b1 b2 c = case c of 
    True = b1
    False = b2

----------------------------------
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)


----------------------------------
