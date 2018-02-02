{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.
    
    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.
    
    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If 
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 = headHunter
headHunter ((x:_):_:_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "Care"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 = firstColumn
firstColumn m = [ x | (x:_) <- m ]

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 = shoutOutLoud
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [ first:first:word | word@(first:_) <- words s ]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces 
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 = pad
pad :: String -> String -> (String, String)
pad s1 s2 = (s1 ++ replicate (longer - length s1) ' ', s2 ++ replicate (longer - length s2) ' ' )
  where longer = max (length s1) (length s2)

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)

-- A median of a list of numbers:
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs


ex422 = quartiles
quartiles :: [Int] -> (Double,Double,Double)
quartiles ns = (q1, q2, q3)
  where q2 = median ns
        half = (length ns) `div` 2
        q1 = median $ fst $ splitAt half $ sort ns
        q3 = median $ snd $ splitAt (half + 1) $ sort ns

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 = pad'
pad' :: String -> String -> (String, String)
pad' s1 s2 = let longer = max (length s1) (length s2) 
  in (s1 ++ replicate (longer - length s1) ' ', s2 ++ replicate (longer - length s2) ' ' )


ex432 = quartiles'
quartiles' :: [Int] -> (Double,Double,Double)
quartiles' ns =
  let q2 = median ns
      half = (length ns) `div` 2
      q1 = median $ fst $ splitAt half $ sort ns
      q3 = median $ snd $ splitAt (half + 1) $ sort ns
  in (q1,q2,q3)

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 :: Show c => (Int, Int) -> [c] -> String
ex441 p (_:z:_) =
  "The pair " ++
  (case p of
    (1,1) -> "contains 2 ones"
    (1,_) -> "contains 1 one"
    (_,1) -> "contains 1"
    (_,_) -> "does not contain a single one" ) ++
  " and the second elem of the list is " ++ (show z)

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 = headsOf
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf (xs:xss) = head xs : headsOf xss 

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
ex521 = modMult
modMult :: Int -> Int -> [Int] -> [Int]
modMult _ _ [] = []
modMult n m (x:xs) = x * n `mod` m  : modMult n m xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 = addPredecessor
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = add 0 xs
  where add n [] = []
        add n (y:ys) = n + y : add y ys

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 = equalTriplets
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((x,y,z): xs) | x == y && y == z = (x,y,z) : equalTriplets xs
                            | otherwise = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Int -> a -> [a]
replicate' n a | n <= 0 = []
               | otherwise = a : replicate' (n-1) a

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 = drop'
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

ex541' = drop''
drop'' :: Int -> [a] -> [a]
drop'' n xs | n < 0 = reverse $ drop' (- n) (reverse xs)
            | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs
   | n1 <= n2 = xs!!n1 : takeFromTo (n1+1) n2 xs
   | otherwise = []

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 = eachThird
eachThird :: [a] -> [a]
eachThird [] = []
eachThird (_:_:x3:xs) = x3 : eachThird xs

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 = crossZip
crossZip :: [a] -> [b] -> [(a,b)]
crossZip _ [] = []
crossZip [] _ = []
crossZip (x:[]) _ = []
crossZip _ (x:[]) = []
crossZip (x:xs) (y:ys) = [(x,head ys), (head xs, y)] ++ crossZip (tail xs) (tail ys)

-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

ex561 = length'
length' :: [a] -> Int
length' xs = mylength xs 0
  where mylength [] n = n
        mylength (x:xs) n = mylength xs (n+1)

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty array"
maxUnzip (x:xs) = helper xs (fst x, snd x)
  where helper [] pair = pair
        helper ((a,b):xs) (a',b') = helper xs (max a a', max b b')

ex562' = maxUnzip'
maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' [] = error "Empty list"
maxUnzip' [x] = x
maxUnzip' ((a,b):xs) = (max a (fst muHelper), max b (snd muHelper))
  where muHelper = maxUnzip' xs