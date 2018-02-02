import Data.Char
import Data.List

-- TYPES --
-- ':t' command and operator '::' determines a type
-- functions also have types e.g. 'lines', 'chr', 'ord', 'toUpper'

addPairs :: [(Int, Int)] -> [Int]
addPairs xs = [ x + y | (x,y) <- xs]


lowerCase :: [Char] -> [Char]
lowerCase s = [toLower c | c <- s]

-- [Char] is equivalent to String

onlyAlpha :: String -> String
onlyAlpha s = [c | c <- s, isAlpha c]

removeEverySecond :: String -> String
removeEverySecond s = 
    unwords [ snd ix | ix <- zip[1..] $ words s, even $ fst ix]

factorial :: Int -> Int
factorial n = product [1..n]

circumference :: Float -> Float -- or Double -> Double
circumference r = 2 * pi * r

-- If a function takes multiple arguments, we can group them in a tuple:

concatThree' :: (String, String, String) -> String
concatThree (s1, s2, s3) = s1 ++ s2 ++ s3

concatThree :: String -> String -> String -> String
concatThree s1 s2 s3 = s1 ++ s2 ++ s3

number :: Int -> Int -> Int
number x y = x * 10 + y

trimBy :: Int -> String -> String
trimBy n xs = reverse $ drop n $ reverse $ drop n xs

-- This way of defining functions of multiple arguments is called CURRIED FORM (writing functions in this way is called CURRYING)
