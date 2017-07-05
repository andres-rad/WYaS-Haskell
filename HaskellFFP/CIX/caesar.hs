module Cipher where

import Data.Char

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip'' xs ys = zipWith' (,) xs ys

------------------------------------
caesar :: String -> Int -> String
caesar "" _ = ""
caesar (x:xs) n = shiftChar x n : caesar xs n

unCaesar :: String -> Int -> String
unCaesar xs n = caesar xs (-n)

shiftChar :: Char -> Int -> Char
shiftChar c n =
  case elem c ['a'..'z'] of
    True  -> chr newChar
             where newChar = mod (ord c + n - ord 'a') 26 + ord 'a'
    False -> chr newChar
             where newChar = mod (ord c + n - ord 'A') 26 + ord 'A'

------------------------------------
myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) = b || myOr bs

-------------------------------------
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr $ map f xs

-------------------------------------
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) = if a == x then True else myElem a xs

-------------------------------------
myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (==a) xs

-------------------------------------
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-------------------------------------
squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

-------------------------------------
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)

-------------------------------------
squishAgain :: [[a]] -> [a]
squishAgain lss = squishMap id lss

-------------------------------------
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:xs) =
  case f x y == GT of
    True  -> myMaximumBy f (x:xs)
    False -> myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:y:xs) =
  case f x y == LT of
    True  -> myMinimumBy f (x:xs)
    False -> myMinimumBy f (y:xs)

-------------------------------------
myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs

-------------------------------------
