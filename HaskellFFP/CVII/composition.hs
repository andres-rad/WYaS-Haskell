{--

(.) :: (b -> c) -> (a -> b) -> (a -> c)

(f . g) x = f (g x)


notación de punto libre

f . g = \x -> f (g x)
f . g . h = \x -> f (g (h x))

--}

cantidad :: Eq a => [a] -> a -> Int
cantidad xs x = length . filter (== x) $ xs
