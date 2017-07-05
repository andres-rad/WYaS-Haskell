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


