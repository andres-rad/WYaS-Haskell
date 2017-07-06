{-# LANGUAGE  ExistentialQuantification #-}

data Mostrable = forall a. Show a => M a

xs :: [Mostrable]
xs = [M 1, M "hola", M True]

mostrar :: [Mostrable] -> String
mostrar xs = concat $ map (\(M x) -> show x) xs