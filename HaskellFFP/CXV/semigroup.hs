import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency, elements)
import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == (a <> (b <> c))

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--------------
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssoc = (Identity Trivial) -> (Identity Trivial) -> (Identity Trivial) -> Bool

--------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return  (Two a b)

type TwoAssoc = (Two String String) -> (Two String String) -> (Two String String) -> Bool

--------------
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = 
        Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c) 

type ThreeAssoc = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

--------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj False) <> _ = BoolConj False
    _ <> (BoolConj False) = BoolConj False
    _ <> _ = BoolConj True  

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return (BoolConj b) 

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True
    _ <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return (BoolDisj b)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd b) <> _ = (Snd b)
    _ <> (Snd a) = (Snd a)
    _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool 

-------------- 
newtype Combine a b =
    Combine { unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
    a <> b = Combine {unCombine = (\x -> (unCombine a) x <> (unCombine b) x)}

--------------
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    a <> b = Comp {unComp = (unComp a) . (unComp b)}

--------------
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup b) => Semigroup (Validation a b) where
    (Failure a) <> _ = (Failure a)
    _ <> (Failure a) = (Failure a)
    (Success b) <> (Success b') = Success (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure a, Success b]

type ValidationAssoc =  Validation String String 
                -> Validation String String 
                -> Validation String String 
                -> Bool

--------------
newtype AccumulateRight a b = 
    AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    a <> AccumulateRight (Failure b) = a
    AccumulateRight (Failure a) <> b = b
    AccumulateRight a <> AccumulateRight b = 
        AccumulateRight (a <> b) 


main = quickCheck (semigroupAssoc :: ValidationAssoc)