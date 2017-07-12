import Control.Monad
import Data.Monoid (mempty, mappend, (<>))
data Nope a =
    NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return x = NopeDotJpg
    (>>=) _ _ = NopeDotJpg  

--------------
data PhbtEither b a = Left' a
                    | Right' b

instance Functor (PhbtEither a) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a)  = Left' $ f a

instance Applicative (PhbtEither a) where
    pure x = Left' x
    (<*>) (Right' b) _ = Right' b
    (<*>) _ (Right' b) = Right' b
    (<*>) (Left' f) (Left' a) = Left' $ f a

instance Monad (PhbtEither a) where
    return = pure 
    (>>=) (Right' b) _ = Right' b
    (>>=) (Left' a) f  = f a

--------------
data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

--------------
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` xs = xs
    xs `mappend` Nil = xs 
    (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)


instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = (f <$> Cons x xs) <> (fs <*> xs)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    Cons x xs >>= f = f x <> (xs >>= f)

--------------
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f 
--meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id 