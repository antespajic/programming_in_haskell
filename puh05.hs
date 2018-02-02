import Data.Char
import Data.List

--1.1
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

--1.2
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf (xs:xss) = head xs : headsOf xss 

modMult :: Int -> Int -> [Int] -> [Int]
modMult _ _ [] = []
modMult n m (x:xs) = x * n `mod` m  : modMult n m xs

addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = add 0 xs
  where add n [] = []
        add n (y:ys) = n + y : add y ys

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((x,y,z): xs) | x == y && y == z = (x,y,z) : equalTriplets xs
                            | otherwise = equalTriplets xs

replicate' :: Int -> a -> [a]
replicate' n a | n <= 0 = []
               | otherwise = a : replicate' (n-1) a

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' 0 xs = xs
drop'' _ [] = []
--drop'' n xs | n < 0 = 

eachThird :: [a] -> [a]
eachThird xs = []

crossZip :: [a] -> [b] -> [(a,b)]
crossZip _ [] = []
crossZip [] _ = []
crossZip (x:[]) _ = []
crossZip _ (x:[]) = []
crossZip (x:xs) (y:ys) = [(x,head ys), (head xs, y)] ++ crossZip (tail xs) (tail ys)

length' :: [a] -> Int
length' xs = mylength xs 0
  where mylength [] n = n
        mylength (x:xs) n = mylength xs (n+1)

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty array"
maxUnzip (x:xs) = helper xs (fst x, snd x)
  where helper [] pair = pair
        helper ((a,b):xs) (a',b') = helper xs (max a a', max b b')
