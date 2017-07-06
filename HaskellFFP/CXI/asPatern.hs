import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l1@(x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
								 | otherwise = isSubsequenceOf l1 ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\(y:ys) -> (y:ys, (toUpper y):ys)) . words


--Language exercises

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = joinWith '.' . map capitalizeWord . splitAtChar '.'


joinWith :: Char -> [String] -> String
joinWith a xs = foldr ((++).(++[a])) [] xs

splitAtChar ::  Char -> String -> [String]
splitAtChar _ "" = []
splitAtChar c xs = takeWhile (/=c) xs : splitAtChar c (drop 1 (dropWhile (/=c) xs)) 

