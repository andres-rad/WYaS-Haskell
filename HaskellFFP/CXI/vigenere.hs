module Vigenere where

import Data.Char

codigo = "MEET AT DAWN"
clave = "ALLY"
decodificar = "MPPR AE OYWY"
--Codificacion vigenere 
vigenere :: String -> String -> String
vigenere code keyword = vigenere' code shiftArray
						where shiftArray = shifts (align code keyword)

--Decodificacion vigenere
unvigenere :: String -> String -> String
unvigenere code keyword = vigenere' code (map negate shiftArray)
						where shiftArray = shifts (align code keyword)

vigenere' :: String -> [Int] -> String
vigenere' "" _ = ""
vigenere' (x:xs) (y:ys) = case x == ' ' of
							True -> ' ' : vigenere' xs ys
							False -> newChar : vigenere' xs ys
									 where newChar = shiftChar x y

--shiftea el Char pasado la cantidad de veces que indique el segundo parametro
--solo para alfabeto. Nunca paso de mayuscula a minuscula.
shiftChar :: Char -> Int -> Char
shiftChar c n =
  case elem c ['a'..'z'] of
    True  -> chr newChar
             where newChar = mod (ord c + n - ord 'a') 26 + ord 'a'
    False -> chr newChar
             where newChar = mod (ord c + n - ord 'A') 26 + ord 'A'

--Genera una lista indicando cuanto hay que shiftear
--el caracter
shifts :: String -> [Int]
shifts = map (\c -> case c `elem` ['a'..'z'] of
				True -> ord c - ord 'a'
				False -> ord c - ord 'A') 

--Alarga el keyword de manera que se alinee con el string
--code
align :: String -> String -> String
align "" _ = ""
align (x:xs) (y:ys) = case x == ' ' of
						True -> ' ' : align xs (y:ys)
						False -> y : align xs (ys ++ [y])  


--{shifts . align code $ keyword} genera como tengo que shiftear cada letra
--de code