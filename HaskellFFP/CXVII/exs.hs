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


--------------
newtype Identity a = Identity a deriving Show

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a


--------------
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)


--------------
data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b) 


--------------
data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three u v f) (Three a b c) = Three (u <> a) (v <> b) (f c)

--------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' u f g) (Three' a b b') = Three' (u <> a) (f b) (g b')

--------------
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four u v w f) (Four a b c d) = Four (u <> a) (v <> b) (w <> c) (f d)


--------------
data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' u v w b) = Four' u v w $ f b

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' u v w f) (Four' x y z b) = Four' (u <> x) (v <> y) (w <> y) (f b)

--------------
stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

showCombos :: [(Char, Char, Char)] -> [String]
showCombos [] = []
showCombos ((c1,c2,c3):cs) = [c1,c2,c3]:(showCombos cs)

ioCombos :: [String] -> IO ()
ioCombos [] = return ()
ioCombos (c:cs) = putStrLn c >> ioCombos cs

comb :: [String]
comb = showCombos $ combos stops vowels stops