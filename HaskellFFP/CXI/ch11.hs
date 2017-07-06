{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Int

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini 
				  | Mazda
				  | Tata
				  deriving (Eq, Show)

data Airline = PapuAir
			 | CatapultsR'Us
			 | TakeYourChamncesUnited
			 deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
			 | Plane Airline Size
			 deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 80)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar 

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m p) = Just m
getManu _ = Nothing

---------------------------------------------
data Example0 = Example0 deriving (Eq, Show)

data Example1 = Example1 Int deriving (Eq, Show)

data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

---------------------------------------------
data Example = MakeExample deriving Show

---------------------------------------------
--data Goats = Goats Int deriving (Eq, Show)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show)


tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
	tooMany :: a -> Bool

instance TooMany Int where
	tooMany n = n > 42


--Intermission: Exercises
---------------------------------------------
instance TooMany (Int, String) where
	tooMany (n, cs) = (n > 42) || (length cs) > 10


--instance TooMany (Int, Int) where
--	tooMany (x, y) = tooMany $ x + y

instance (Num a, TooMany a) => TooMany (a, a) where
	tooMany (n, m) =  tooMany $ n + m

---------------------------------------------
data BigSmall = Big Bool
			  | Small Bool
			  deriving (Eq, Show)

-- #BigSmall = #Bool | #Bool
--			 = #Bool + #Bool
--			 = 2 + 2 
--           = 4

data NumberOrBool = Numba Int8
				  | BoolyBool Bool
				  deriving (Eq, Show)

-- #NumberOrBool = #Int8 | #Bool
--				 = #Int8 + #Bool
--				 = 256 + 2
--				 = 258

myNumba = Numba (-128)

--Product types
---------------------------------------------

data QuantumBool = QuantumTrue
				 | QuantumFalse
				 | QuantumBoth 
				 deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool 
		    deriving (Eq, Show)

-- #TwoQs = #QuantumBool * #QuantumBool
--		  = 3 * 3
--		  = 9

--Record Syntax
--------------------------------------------
data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chirs" 16

namae :: Person -> String
namae (MkPerson s _) = s

data Persona =
	Persona { name :: String
			, age :: Int }
			deriving (Eq, Show)

-- :t name
-- name :: Person -> String 






