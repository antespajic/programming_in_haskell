import Data.Char
import Prelude hiding (foldr,foldl)
import Data.List
import Control.Monad
import Data.Ord (comparing)

-- Define 'sumEven' that adds up elements occurring at even (incl. zero) 
-- positions in a list. 
-- sumEven [1..10] => 25
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..] 

-- Define 'filterWords ws s' that removes from string 's' all words contained
-- in the list 'ws'.
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (flip notElem ws) . words


-- Define 'maxDiff xs' that returns the maximum difference between consecutive
-- elements in the list 'xs'.
-- maxDiff [1,2,3,5,1] => 4
-- - Define 'maxMinDiff' that returns the pair (min_difference, max_difference).
maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) $ zip (tail xs) xs

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (diff minimum xs, diff maximum xs)
  where diff f xs = f . map (abs . uncurry (-)) $ zip (tail xs) xs

-- Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
-- returns the names of all students who scored at least 50% of the maximum 
-- score.
type NameSurname = String
type Score = Int
-- studentsPassed :: [(String, Int)] -> [String]
-- studentsPassed xs = map fst . filter ((>=LIMES xs) . snd)
--   where LIMES xs = maximum . map snd xs

-- Define 'isTitleCased' that checks whether every word in a string is
-- capitalized.
isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) . words

-- Define 'sortPairs' that sorts the list of pairs in ascending order with
-- respect to the second element of a pair.
sortPairs :: Ord a => [(b,a)] -> [(b,a)]
sortPairs = sortBy (comparing snd)

-- FOLDR == REDUCE from right
-- FOLDL == REDUCE from left

-- Define 'elem' using 'foldr'.
elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\x acc -> acc || x == e) False

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x] ) []

-- nubRuns "Mississippi" => "Misisipi"
-- nubRuns :: Eq a => [a] -> [a]
-- nubRuns = foldr (\x acc -> [x] ++ acc )

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x: acc ) []

-- Using 'foldl' define function 'sumEven' from problem 1.1.
-- sumEven = sum . map snd . filter (even . fst) . zip [0..]
sumEven' :: [Integer] -> Integer 
sumEven' = foldl (\acc x -> if even (fst x) then acc + (snd x) else acc ) 0 . zip [0..]

maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip = foldl1 (\(accx, accy) (x,y) -> (max accx x, max accy y) )