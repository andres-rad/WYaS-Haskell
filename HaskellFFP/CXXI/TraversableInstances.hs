data Opcional a b = Der a
                  | Izq b
                  deriving (Eq, Ord, Show)

instance Functor (Opcional a) where
    fmap _ (Der x) = Der x
    fmap f (Izq y) = Izq $ f y

instance Applicative (Opcional a) where
    pure = Izq
    Der e <*> _ = Der e
    Izq f <*> r = fmap f r

instance Foldable (Opcional a) where
    foldMap _ (Der _) = mempty
    foldMap f (Izq y) = f y

    foldr _ z (Der _) = z
    foldr f z (Izq y) = f y z

instance Traversable (Opcional a) where
    traverse _ (Der x) = pure (Der x)
    traverse f (Izq y) = Izq <$> f y

--------------
data Two a b = Two a b deriving (Eq, Ord, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    Two u f <*> Two v x = Two (u `mappend` v) (f x)

instance Foldable (Two a) where
    foldMap f (Two _ y) = f y
    
    foldr f z (Two _ y) = f y z

instance Traversable (Two a) where
    traverse f (Two x y) = Two x <$> f y

--------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldr f acc (Identity a) = f a acc

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

--------------
newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant {getConstant = x}) = Constant {getConstant = x}

instance Foldable (Constant a) where
    foldr _ acc _ = acc

instance Traversable (Constant a) where
    traverse _ (Constant {getConstant = x}) = pure $ Constant {getConstant = x}


--------------
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    
    Nil `mappend` xs = xs
    xs `mappend` Nil = xs
    Cons x xs `mappend` ys = Cons x $ xs `mappend` ys

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs 

instance Foldable List where
    foldr _ acc Nil = acc
    foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons x xs = (f <$> Cons x xs) `mappend` (fs <*> Cons x xs)

instance Traversable List where
    traverse f Nil = pure Nil
    traverse f (Cons a xs) = undefined    
--traverse :: (a -> f b) -> t a -> f (t b)
