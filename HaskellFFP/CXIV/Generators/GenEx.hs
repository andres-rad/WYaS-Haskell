module GenEx where

import Test.Hspec
import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

genEqFool :: Gen Fool
genEqFool = elements [Fulse, Frue]

genSkewedFool :: Gen Fool
genSkewedFool = frequency [(2, return Fulse), (1, return Frue)]