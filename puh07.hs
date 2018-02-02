
import Data.Char
import Prelude hiding (map, filter)
import Data.List

-- exercise 1

takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

index :: [a] -> [(Int, a)]
index = zip [0..]

index' :: [a] -> [(a, Int)]
index' = (`zip` [0..])

divider :: Int -> String
divider = (`replicate` '=')

-- exercise 2


applyOnLast f xs ys = last xs `f` last ys

addThree :: Num a => a -> a -> a -> a   -- or: a -> (a -> (a -> a))
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = applyOnLast (addThree 100) xs ys

applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes n f x 
  | n > 0 = applyManyTimes (n-1) f (f x)
  | otherwise = x

applyTwice' :: (a -> a) -> a -> a
applyTwice' = applyManyTimes 2

listifylist :: [a] -> [[a]]
listifylist = map (:[])

cutoff :: Int -> [Int] -> [Int]
cutoff n = map $ min n

sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

freq :: Eq a => a -> [a] -> Int
-- freq 'k' "kikiriki" => 3
freq c xs = length $ filter (==c) xs

-- freqFilter :: Eq a => Int -> [a] -> [a]
-- freqFilter 4 "kikiriki" => "iiii"
-- freqFilter n xs = 

withinInterval n m = filter (\x -> x >= n && x<= m)

-- sndColumn :: [[a]] -> [a] 
-- sndColumn m = filter (\xs -> elemIndex xs == 1) m