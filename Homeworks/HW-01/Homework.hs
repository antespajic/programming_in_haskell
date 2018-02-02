module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
isLeapYear :: Int -> Bool
isLeapYear year = year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 == 0

leapList :: [Int]
leapList = [x | x <- [1996..2017], isLeapYear x]

-- Task 02
evaluate :: Double -> [Double] -> Double
evaluate x as = sum [ (snd p)*x^(fst p)| p <- zip [0..] as]

factorial :: Double -> Double
factorial n = product [1..n]

maclaurin :: [Double]
maclaurin = [1/(factorial n) | n <- [0,1..]]

exp' :: Double -> Double
exp' n = evaluate n $ take 170 maclaurin

-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem elems key = [elem | elem <- elems, (fst elem) == key]

contains :: [(String, a)] -> String -> Bool
contains elems key = not $ null $ findItem elems key

lookup :: [(String, a)] -> String -> a
lookup elems key = if contains elems key then snd $ head $ findItem elems key else error "No such key present"

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert elems newElem = if not $ contains elems $ fst newElem then elems ++ [newElem] else elems

remove :: [(String, a)] -> String -> [(String, a)]
remove elems key  = [pair | pair <- elems, (fst pair) /= key]

update :: [(String, a)] -> String -> a -> [(String, a)]
update elems key newValue = if contains elems key then Homework.insert (remove elems key) (key, newValue) else elems

-- Task 04
cosineSimilarity :: String -> String -> Double
cosineSimilarity = undefined