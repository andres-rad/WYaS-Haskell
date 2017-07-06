import Data.Char
import Data.List
import Data.Maybe

data Digit = Zero | One | Two | Three | Four | Five 
			| Six | Seven | Eight | Nine
			| Star | Num
			deriving (Eq, Show)

data Button = Button Digit String deriving (Eq, Show)
type Phone = [Button]
type Presses = Int 

digit :: Button -> Digit
digit (Button d _) = d

stringButton :: Button -> String
stringButton (Button _ s) = s


phone :: Phone
phone = [Button One "1"      , Button Two "abc2"   , Button Three "def3"
		,Button Four "ghi4"  , Button Five "jkl5"  , Button Six "mno6"
		,Button Seven "pqrs7", Button Eight "tuv8" , Button Nine "wxyz9" 
		,Button Star "^"     , Button Zero " 0"     , Button Num ".,"]

convo :: [String]
convo =
	["Wanna play 20 questions",
	 "Ya",
	 "U 1st haha",
	 "Lol ok. Have u ever tasted alcohol lol",
	 "Lol ya",
	 "Wow ur cool haha. Ur turn",
	 "Ok. Do u think I am pretty Lol",
	 "Lol ya",
	 "Haha thanks just making sure rofl ur turn"]

--Chars producibles por el telefono
safeChars :: [Char]
safeChars = ' ' : '.' : ',' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

--Chars que solo tengo que apretar un boton n veces
directChar :: [Char]
directChar = ' ' : '.' : ',' : ['a'..'z'] ++ ['0'..'9']

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p c 
				| not (c `elem` safeChars) = error "bleh"
				| c == ' ' = [(Zero, 1)] 
				| c `elem` ['A'..'Z'] = (Star, 1) : reverseTaps p (toLower c)
				| c `elem` directChar = code p c : []

--Dado un telefono y un safeChar me dice que botones tengo que apretar y cuantas veces
code :: Phone -> Char -> (Digit, Presses)
code p@(b:bs) c  
      | null p = error "telefono mal"
      | c `elem` str = (digit b, 1 + fromJust (elemIndex c str))
      | otherwise    = code bs c
      where str = stringButton b

--Traduce una oracion
taps :: Phone -> String -> [(Digit, Presses)]
taps p s = concatMap (reverseTaps p) s  

--Traduce una lista de oraciones
tapsConv :: Phone -> [String] -> [[(Digit, Presses)]]
tapsConv p s = map (taps p) s


