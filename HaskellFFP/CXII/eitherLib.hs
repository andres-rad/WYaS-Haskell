lefts' :: [Either a b] -> [a]
lefts' = map fromLeft . filter isLeft

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "bleh"

isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft _ = False

rights' :: [Either a b] -> [b]
rights' = map fromRight . filter (not . isLeft)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "bleh"

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = \xs -> (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b 

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (\e -> Just (f e))