data ThereYet = There Integer Float String Bool deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusss :: ThereYet
yusss = notQuite False


--Deconstructing values
---------------------------------------------

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
				| WheatFarmer
				| SoybeanFarmer
				deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
	FarmerRec { name :: Name
			  , acres :: Acres
			  , farmerType :: FarmerType }
			  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
								DairyFarmer -> True
								_ -> False