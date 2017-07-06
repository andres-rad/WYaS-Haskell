isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee nil f Nothing = nil
mayybee nil f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe base = mayybee base id 

listToMaybe :: [a] -> Maybe a
listToMaybe = \x -> if null x then Nothing else Just $ head x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[]) 

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap (\x -> if isNothing x then [] else maybeToList x)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = if all isJust ms then Just (concatMap maybeToList ms) else Nothing

