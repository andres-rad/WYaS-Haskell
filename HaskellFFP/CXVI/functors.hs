data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a | WhathIsCalled deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhathIsCalled = WhathIsCalled
    fmap f (Matter a) = Matter (f a)