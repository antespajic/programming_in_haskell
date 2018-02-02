module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec x -> if x == 0 then 1 else x * rec(x-1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\rec xs -> if null xs then 0 else (head xs) + rec(tail xs))

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' n = fix (\rec x n -> if x == 0 then n else rec (x-1) n*x ) n 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' xs = fix (\rec xs n -> if null xs then n else rec (tail xs) n + head xs ) xs 0

nats :: [Integer]
nats = fix (\rec x -> x: rec (x+1)) 1

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec xs -> if null xs then [] else f(head xs):rec (tail xs))

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\rec xs ys -> if null xs || null ys then [] else (head xs, head ys):rec (tail xs) (tail ys))

-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n xs = withFst (nub xs) ++ withoutFst (nub xs)
  where withFst (x:rest) = map (x:) (subsets (n-1) rest)
        withoutFst (_:rest) = subsets n rest

partitions :: [a] -> [[[a]]]
partitions = undefined

-- Task 03
permutations' :: [a] -> [[a]]
permutations' = undefined