module MidtermExam where
--
import Data.List
import Data.Char (toLower)
--

{-
	Problem 01 - (3 points)

	Write a function `listOfDigits :: [Int] -> [Int]` which takes a list of
	nonnegative integers and produces a list of their consecutive digits.
-}

digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

listOfDigits :: [Int] -> [Int]
listOfDigits [] = []
listOfDigits (x:xs) = digits x ++ listOfDigits xs

{-
	Tests for Problem 01
-}

p1t1 :: Bool
p1t1 = listOfDigits [123,375,0,42] == [1,2,3,3,7,5,0,4,2]

p1t2 :: Bool
p1t2 = take 20 (listOfDigits [1..]) == [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1]

--

{-
	Problem 02 - (4 points)

	Implement the function `anagramsFor :: String -> [String] -> [String]`
	that, given a word and a list of possible anagrams, select the correct
	sublist.

	Given "listen" and a list of candidates like "enlists" "google" "inLets"
	"banana" the program should return a list containing "inLets".

	NOTE: The input is NOT case-sensitive. "INLetS" and "inLeTs" should be
	the same.
-}

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (isAnagram word)

isAnagram :: String -> String -> Bool
isAnagram a b = sort (toLowerString a) == sort (toLowerString b)

toLowerString :: String -> String
toLowerString = map toLower

{-
	Tests for Problem 02
-}

p2t1 :: Bool
p2t1 = anagramsFor "listen" ["enlists", "google", "inlets", "banana", "letins"] == ["inlets", "letins"]

p2t2 :: Bool
p2t2 = anagramsFor "listen" ["enlists", "google", "inLets", "banana"] == ["inLets"]

--

{-
	Problem 03 - (3 points)

	Define a triangle data type which stores length of its 3 sides as `Int`.
	To make sure proper triangle is created provide a "constructor" function
	`makeTriangle :: Int -> Int -> Int -> Maybe Triangle` which checks if
	triangle is valid and than returns just a new triangle or nothing
	otherwise.

	Triangle is valid if any side is less than the sum of the other two sides.

	Here is a definition of `Maybe` data type (which is already included in
	`Prelude`).

	data Maybe a = Just a | Nothing
-}

data Triangle = Triangle Int Int Int

notValid :: (Ord a, Num a) => a -> a -> a -> Bool
notValid a b c = (a + b < c) || (a + c < b) || (b + c < a)

makeTriangle :: Int -> Int -> Int -> Maybe Triangle
makeTriangle a b c = if notValid a b c then Nothing else Just $ Triangle a b c

{-
	Tests for Problem 03
-}

p3t1 :: Bool
p3t1 = case makeTriangle 100 20 20 of { Nothing -> True; Just _ -> False; }

p3t2 :: Bool
p3t2 = case makeTriangle 3 4 5 of { Nothing -> False; Just _ -> True; }
