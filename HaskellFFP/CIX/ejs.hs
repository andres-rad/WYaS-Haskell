filterLowerCase :: String -> String
filterLowerCase xs = filter (not . isUpper) xs

--------------------------------
filterUpperCase :: String -> String
filterUpperCase xs = filter isUpper xs

--------------------------------
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

--------------------------------
capitalize' :: String -> String
capitalize' "" = ""
capitalize' "woot" = "WOOT"
capitalize' (x:xs) = toUpper x : xs

--------------------------------
capitalizeHead :: String -> Char
capitalizeHead xs = toUpper . head $ xs

capitalizeHeadPF :: String -> Char
capitalizeHeadPF = toUpper . head
--------------------------------
