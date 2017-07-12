import Control.Applicative
import Data.Monoid (mempty, mappend, (<>))
-- import Test.QuickCheck
-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes 

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
    fmap f (Cons a xs) = Cons (f a) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) (Cons x xs) = (f <$> (Cons x xs)) <> (fs <*> (Cons x xs)) 

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--     xs =-= ys = xs' `eq` ys'
--         where xs' = let (ZipList' l) = xs
--                     in take' 3000 l
--               ys' = let (ZipList' l) = ys
--                     in take' 3000 l

instance Monoid a => Monoid (ZipList' a) where
    mempty = ZipList' Nil
    mappend = liftA2 mappend
        
instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' (Cons x Nil)
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = 
        ZipList' $ paralellApply (Cons f fs) (Cons x xs)

paralellApply :: List (a -> b) -> List (a) -> List (b)
paralellApply Nil _ = Nil
paralellApply _ Nil = Nil
paralellApply (Cons f fs) (Cons x xs) = Cons (f x) $ paralellApply fs xs


--------------
data Validation err a = Failure err
                      | Success a
                      deriving (Eq, Show)

data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
            deriving (Eq, Show)

instance (Monoid e, Monoid a) => Monoid (Validation e a) where
    mempty = Success mempty
    mappend (Failure e) (Success a) = Failure e
    mappend (Success a) (Failure e) = Failure e
    mappend (Failure e) (Failure e') = Failure (e <> e')
    mappend (Success a) (Success b) = Success (a <> b)

instance Functor (Validation e) where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Failure e) (Failure e') = Failure $ e <> e'
    (<*>) (Failure e) (Success _)  = Failure e
    (<*>) (Success _) (Failure e)  = Failure e
    (<*>) (Success f) (Success a)  = Success $ f a 


--------------
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum b) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum b) where
    pure = Second
    (<*>) (First a) _ = (First a)
    (<*>) _ (First a) = (First a)
    (<*>) (Second f) (Second b) = Second $ f b