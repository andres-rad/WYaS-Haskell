import Data.Monoid (Any)

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

--------------
data Optional a = Nada
                | Yep a
                deriving (Eq, Show)

instance Foldable Optional where
    foldr _ z Nada    = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada    = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

--------------
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0 

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr ((||) . (==e)) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x y -> case y of
                            Nothing -> Just x
                            Just a -> Just $ min x a) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (\x y -> case y of
                            Nothing -> Just x
                            Just a -> Just $ max x a) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr ((&&) . const False) True

length :: (Foldable t) => t a -> Int
length = foldr ((+) . const 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (id)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty 

--------------
data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f acc _ = acc 

--------------
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f acc (Two a b) = f b acc

--------------
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f acc (Three a b c) = f c acc

--------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f acc (Three' a b b') = f b' $ f b acc 

--------------
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
    foldr f acc (Four' a b b1 b2) = f b2 (f b1 (f b acc))


--------------
filterF :: (Applicative f, Foldable f, Monoid (f a)) => 
           (a -> Bool) -> f a -> f a

filterF f = foldMap (\x -> if f x then pure x else mempty)

