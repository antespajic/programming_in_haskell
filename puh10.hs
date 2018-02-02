import Data.List
import Control.Monad

data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

judith = Person2 "223" "Judith" "Rieser" Female Nothing Nothing Nothing [judith]
john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]


parentCheck :: Person2 -> Bool
parentCheck p = undefined --elem p $ children2 . mother2 p

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

l1 = 1 `Cons` Empty
l2 = 1 `Cons` (2 `Cons` (3 `Cons` Empty))

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (x `Cons` xs) = Just x

listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (x `Cons` xs) = f x `Cons` listMap f xs