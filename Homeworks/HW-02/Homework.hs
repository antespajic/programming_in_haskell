module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
-- Takes the DNA string and transforms it into its RNA counterpart
toRNA :: String -> String
toRNA s = [ transform $ toUpper c | c <- s ]
  where transform 'G' = 'C'
        transform 'C' = 'G'
        transform 'T' = 'A'
        transform 'A' = 'U'
        transform _ = error "Nucleotide does not exist"

-- Task 02
-- Multiplies 2 numbers in terms of addition
multiply :: Int -> Int -> Int
multiply _ 0 = 0
multiply 0 _ = 0
--multiply x y = x + multiply x (y-1) -- classic recursion
multiply x y = mulhelper x y 0 -- accumulator version (more efficient)
  where mulhelper _ 0 n = n
        mulhelper a b n = mulhelper a (b-1) (n+a)

-- Divides 2 numbers in terms of substraction
divide :: Int -> Int -> Int
divide _ 0 = error "Division by zero"
divide 0 _ = 0
divide x y = divhelper x y 0
  where divhelper a b n
          | a == 0 = n
          | a < 0 || b < 0 = divhelper (a+b) b (n-1)
          | otherwise = divhelper (a-b) b (n+1)

-- Finds a greatest common divisor of 2 numbers
greatestCD :: Int -> Int -> Int
greatestCD x 0 = x
greatestCD x y = greatestCD y (x `mod` y)

-- Task 03
numberToWords :: Int -> String
numberToWords 0 = ""
numberToWords 1 = "one"
numberToWords 2 = "two"
numberToWords 3 = "three"
numberToWords 4 = "four"
numberToWords 5 = "five"
numberToWords 6 = "six"
numberToWords 7 = "seven"
numberToWords 8 = "eight"
numberToWords 9 = "nine"
numberToWords 10 = "ten"
numberToWords 11 = "eleven"
numberToWords 12 = "twelve"
numberToWords 13 = "thirteen"
numberToWords 15 = "fifteen"
numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "forty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"
numberToWords 100 = "hundred"

-- numberToWords n = ntwHelper (digs n)
--   where ntwHelper (n:ns) 
--           | length ns > 5 = numberToWords n ++ "million" ++ numberToWords ns
--           | length ns > 4 = 


-- Helper function that transforms a number into a list of digits
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]


-- Task 04
undefined' :: a
undefined' = undefined' -- you can't use this definition obviously, make your own :)
