module Reverse where

rvrs :: String -> String
rvrs cs = (drop 9 cs) ++ " " ++ (take 3 $ drop 6 cs) ++ (take 5 cs)

main :: IO ()
main = print $ rvrs "Curry is awesome"