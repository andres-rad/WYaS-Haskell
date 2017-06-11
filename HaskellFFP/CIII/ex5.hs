rvrs :: [Char] -> [Char]
rvrs cs = (drop 9 cs) ++ " " ++ (take 3 $ drop 6 cs) ++ (take 5 cs)