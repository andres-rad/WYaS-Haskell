--Intermission: Jammin Exercises
---------------------------------------------
module Jammin where

import Data.Function
import Data.List


data Fruit = Peach
		   | Plum
		   | Apple
		   | Blackberry
		   deriving (Eq, Show, Ord)

data JamJars = 
	Jam { fruit :: Fruit
	    , amount :: Int }
	    deriving (Eq, Show, Ord)

-- #JamJars = #Fruit * #Int

row1 = Jam Peach 20
row2 = Jam Peach 20
row3 = Jam Apple 30
row4 = Jam Apple 12
row5 = Jam Plum 12
row6 = Jam Blackberry 100

allJam = [row1, row2, row3, row4, row5, row6]

nJams :: [JamJars] -> Int
nJams = length

mostRow :: [JamJars] -> JamJars
mostRow xs = foldr (\x y -> if  ((>) `on` amount) x y then x else y) (Jam Peach 0) xs

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy (compare `on` fruit) 


--Normal form
----------------------------------------------
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
			  | NonfictionBook Nonfiction
			  deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

data Autor = Fiction' AuthorName
		   | Nonfiction' AuthorName
		   deriving (Eq, Show)

data Expr = Number Int
		  | Add Expr Expr
		  | Minus Expr
		  | Mult Expr Expr
		  | Divide Expr Expr

{--

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr =
	Either Number
	 (Either Add
	  (Either Minus
	   (Either Mult Divide)))
--}

--Exercises
---------------------------------------------
data FlowerType = Gardenia
				| Daisy
				| Rose
				| Lilac
				deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

{--
Normal form:

data Garden = Gardenia Garden
			| Daisy Garden
			| Rose Garden
			| Lilac Garden
			deribing Show
--}


--Constructing and deconstructing values
---------------------------------------------
data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a 
			 | Second b
			 deriving (Eq, Show)

data RecordProduct a b =
	RecordProduct { pfirst :: a
				  , psecond :: b }
				  deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
newtype NumSheep = NumSheep Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
data BigFarmHouse = BigFarmHouse NumCow NumPig NumSheep deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig
type BigFarmHouse' = Product NumCow (Product NumPig SheepInfo)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo
		    | Pig PigInfo
		    | Sheep SheepInfo
		    deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


--Constructing values

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter' = Twitter' deriving (Eq, Show)
data AskFm' = AskFm' deriving (Eq, Show)

socialNetwork :: Sum Twitter' AskFm'
socialNetwork = First Twitter'

data SocialNetwork = Twitter | AskFm deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42, psecond = 0.00001}

data OperatingSystem = GnuPlusLinux
					 | OpenBSDPlusNevermindJustBSDStill
					 | Mac
					 | Windows
					 deriving (Eq, Show)

data ProgrammingLanguage = Haskell
						 | Agda
						 | Idris
						 | PureScript
						 deriving (Eq, Show)

data Programmer =
	Programmer { os :: OperatingSystem
			   , lang :: ProgrammingLanguage }
			   deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

--Excercise
---------------------------------------------
allOperatingsystems :: [OperatingSystem]
allOperatingsystems =
	[ GnuPlusLinux
	, OpenBSDPlusNevermindJustBSDStill
	, Mac
	, Windows
	]

allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = x, lang = y } | x <- allOperatingsystems, y <- allLanguages]


