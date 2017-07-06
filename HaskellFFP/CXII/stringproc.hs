import Data.Maybe
import Data.List 
import Data.Char
--1
----------------------------------
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . (map (\x -> if x == Nothing then "a" else fromJust x) . wordss)
            where wordss = (map notThe) . words 

--2
----------------------------------
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = counter . words 

counter :: [String] -> Integer
counter [] = 0
counter [x] = 0
counter (x:y:xs) = case x == "the" of
                    True -> case (y !! 0) `elem` vowels of
                        True -> 1 + counter xs
                        False -> counter (y:xs)
                    False -> counter (y:xs)  

--3
----------------------------------
countVowels :: String -> Integer
countVowels = foldr ((+) . (\x -> if x `elem` vowels then 1 else 0)) 0

countConsonants :: String -> Integer
countConsonants = \x -> toInteger (length x) - countVowels x

--4 (Validate the word)
----------------------------------
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs = case countVowels xs <= countConsonants xs of
                True -> Just (Word' xs)
                False -> Nothing

--5 (It's only Natural)
----------------------------------
data Nat = Zero | Succ Nat
        deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n = case n < 0 of 
                    True -> Nothing
                    False-> Just (integerToNat' n)

integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = Succ(integerToNat' $ n-1) 
