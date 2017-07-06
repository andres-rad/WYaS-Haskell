module WordNumber where

import Data.List (intersperse)
import Test.Hspec

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "no se"

digits :: Int -> [Int]
digits n = case (div n 10 == 0) of
    True  -> [n]
    False -> digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))

main :: IO ()
main = hspec $ do
    describe "digitToWord does what we want" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "return one for 1" $ do
            digitToWord 1 `shouldBe` "one"

    describe "digits does what we want" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1, 0, 0]

    describe "wordNumber does what we want" $ do
        it "returns one-zero-zero for 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "return nine-zero-zero-one for 100" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"