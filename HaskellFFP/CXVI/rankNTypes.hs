data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


----------
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b


---------
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a