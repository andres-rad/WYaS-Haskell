data Trivial = 
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

--------------------------------
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon    = True
    (==) Tue Tue    = True
    (==) Weds Weds  = True
    (==) Thu Thu    = True
    (==) Fri Fri    = True
    (==) Sat Sat    = True
    (==) Sun Sun    = True
    (==) _  _       = False

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

--------------------------------
data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

--------------------------------
data TisAnInteger = 
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn n) (TisAn m) = n == m

--------------------------------
data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two n1 n2) (Two m1 m2) = (n1 == m1 && n2 == m2)

--------------------------------
data StringOrInt =
      TisAnInt Int 
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt n) (TisAnInt m)       = n == m
    (==) (TisAString s1) (TisAString s2) = s1 == s2
    (==) _ _                             = False

----------------------------------
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = (x == x' && y == y')

----------------------------------
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (a == a' && b == b')

----------------------------------
data Which a =
     ThisOne a
    |ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _                     = False

----------------------------------
data EitherOr a b =
     Hello a
    |Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y)     = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _                     = False









