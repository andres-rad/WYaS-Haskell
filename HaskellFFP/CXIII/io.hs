import Control.Monad
import Data.Char (toLower, isAlpha)

main1 :: IO Bool
main1 =
     do c  <- getChar
        c' <- getChar
        return $ c == c'

----------------------------------
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (isPalindrome line1) of
        True -> putStrLn "It's a palindrome!\n"
        False -> putStrLn "Nope!\n"

isPalindrome :: String -> Bool
isPalindrome cs = csFlat == reverse csFlat
                where csFlat = flatten cs

flatten :: String -> String
flatten cs = map toLower $ filter isAlpha cs

----------------------------------
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name  
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                        "Name was: " ++ show name ++
                        " Age was: " ++ show age

nameError :: String
nameError = "Name can't be an empty string"

ageError :: String
ageError = "Age must be greater than 0"

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter your name: "
    name <- getLine
    putStr "Now enter your age: "
    age <- getLine
    case mkPerson name (read age) of
        Right newPerson ->
            putStrLn $ "Yay! Succesfully got a person: " ++ show newPerson
        Left NameEmpty ->
            putStrLn nameError
        Left AgeTooLow ->
            putStrLn ageError
        Left (PersonInvalidUnknown cs) ->
            putStrLn cs

