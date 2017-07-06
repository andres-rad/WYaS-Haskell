module QTestCheck where

import Test.QuickCheck
import Test.Hspec
import Data.List (sort)

--------------
half :: Float -> Float
half x = x / 2

halfIdentity = (*2) . half

--------------
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

--------------
plusAsociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y =
    x + y == y + x

--------------
multAsociative x y z =
    x * (y * z) == (x * y) * z

multCommutative x y =
    x * y == y * x

--------------
inverseQuotRem x y = 
    y == 0 || (quot x y ) * y + (rem x y) == x 

inverseDivMod x y = 
    y == 0 || (div x y) * y + (mod x y) == x

--------------
reverseSquared xs = (reverse . reverse) xs == xs

--------------
application_id f g x =
    (f $ g $ x) == f(g x) && (f $ g $ x) == (f. g $ x)


main :: IO ()
main = hspec $ do
    describe "exercise quick test" $ do
        it "(*2) . half = id" $ do
            property (\x -> halfIdentity x == (x::Float))
        it "sort always orders a list" $ do
            property (\xs -> listOrdered $ sort (xs::[Int]))
        it "+ is associative" $ do
            property (\x y z -> plusAsociative (x::Int) (y::Int) (z::Int))
        it "+ is commutative" $ do
            property (\x y -> plusCommutative (x::Int) (y::Int))
        it "* is associative" $ do
            property (\x y z -> multAsociative (x::Int) (y::Int) (z::Int))
        it "* is commutative" $ do
            property (\x y -> multCommutative (x::Float) (y::Float))
        it "Quot + Rem de x y == x (y /= 0)" $ do
            property (\x y -> inverseQuotRem (x::Int) (y::Int))
        it "Div + Mod de x y == x (y /= 0)" $ do
            property (\x y -> inverseDivMod (x::Int) (y::Int))
        it "reverse . reverse = id" $ do
            property (\xs -> reverseSquared (xs::[Char]))
        it "associativity of ($)" $ do
            property (\x -> application_id (+2) (+1) (x :: Int))
