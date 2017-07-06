import Data.Time

data DatabaseItem = DbString String
				  | DbNumber Integer
				  | DbDate UTCTime
				  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
	[ DbDate (UTCTime
				(fromGregorian 1991 5 1)
				(secondsToDiffTime 34123))
	, DbNumber 9001
	, DbString "Hello, world!"
	, DbDate (UTCTime
				(fromGregorian 1921 5 1)
				(secondsToDiffTime 34123))
	]


--------------------------
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map getUTCTime $ filter isUTCTime db

--------------------------
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map getInteger (filter isDbNumber db)

--------------------------
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum $ filterDbDate db

--------------------------
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 $ filterDbNumber db

--------------------------
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)


--Funciones Auxiliares

isUTCTime :: DatabaseItem -> Bool
isUTCTime (DbDate _) = True
isUTCTime _ 		 = False
 
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _			= False

getUTCTime :: DatabaseItem -> UTCTime
getUTCTime (DbDate x) = x
getUTCTime _		  = UTCTime (fromGregorian 1700 1 1) (secondsToDiffTime 34123)

getInteger :: DatabaseItem -> Integer
getInteger (DbNumber n) = n
getInteger _			= (-1)
