import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency, elements)
import Data.Semigroup

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada o = o
    mappend o Nada = o
    mappend (Only a) (Only b) = Only (mappend a b)


--------------
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == (a <> (b <> c))

monoidAssoc :: (Eq m, Monoid m, Semigroup m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


--------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


--------------
newtype Identity a = Identity a deriving (Eq, Show) 

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)


--------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)


--------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj False) <> _ = BoolConj False
    _ <> (BoolConj False) = BoolConj False
    _ <> _ = BoolConj True  

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return (BoolConj b) 


--------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)


instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True
    _ <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return (BoolDisj b)


--------------
newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    a <> b = Combine {unCombine = (\x -> (unCombine a) x <> (unCombine b) x)}

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine { unCombine = const mempty }
    mappend = (<>)


--------------
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    a <> b = Comp {unComp = (unComp a) . (unComp b)}

instance Monoid a => Monoid (Comp a) where
    mempty = Comp { unComp = id }
    mappend = (<>)


--------------
newtype Mem s a =
    Mem { runMem :: s -> (a, s) }

instance (Semigroup s, Monoid a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    
    mappend Mem {runMem = f} Mem {runMem = g} = 
        Mem $ \x -> let (a, b) = g x 
                        (c, d) = f b
                    in (a `mappend` c, d)

--------------

f' = Mem $ \s -> ("hi", s ++ "a")

main :: IO ()
main = do
    print $ runMem (f' `mappend` mempty) ""
    print $ runMem (mempty `mappend` f') ""
    print $ (runMem mempty "" :: (String, String))
    print $ runMem (f' `mappend` mempty) "" == runMem f' ""
    print $ runMem (mempty `mappend` f') "" == runMem f' ""

{-
main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
-}