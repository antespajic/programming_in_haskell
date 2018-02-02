import Data.Char
import Data.List

-- Replicate, cycle, and repeat

l9 = repeat 'a'
l10 = cycle [1,2,3]
l11 = replicate 10 'a'

-- How to implement replicate with repeat?

replicate' n x = take n $ repeat x

-- List intervals

l12 = [1..100]

l13 = [1,3..999]

l14 = take 10 [1,3..100]

l15 = [1..]

l16 = ['a'..'z']

-- Laziness in action:

l17 = take 10 [1..]
l18 = head [1..]

-- What's going on here?

l19 = tail [1..]
n = length [1..]

trim l = tail $ init l
trim' l = init $ tail l

blanks =  repeat ' '

padTo10 s = s ++ take (10 - length s) blanks

-- Be careful with this

l20 = head []

l21 = [[1,2,3],[4,5,6],[7,8,9,10]]
l22 = ["red", "green", "blue"]

-- Lists can't be heterogenous

-- Concatenating lists of sublists:

l23 = concat l21

-- Minimum and maximum of a list:

m1 = minimum [1,2,3]
m2 = maximum "Haskell for the win!"

-- Looking up elements from a list

e1 = [1,3..100] !! 17

e2 = l21 !! 1 !! 2

-- Our own implementation of 'chr':

intToChar i = ['A'..] !! (i - 65)

intToChar' i | i >= 65 = ['A'..] !! (i-65)
             | otherwise = error "Index should be greater than -1"

-- Logical operators on lists:

r1 = and [True,True,False]
r2 = or [True,True,False]

-- Removing duplicates with 'nub'

l24 = nub [1, 2, 3, 1, 1, 2]
l25 = nub "Give me every letter only once"

-- Sorting a list

l26 = sort [1,4,5,6,1,2]
l27 = sort "Alphabet"

-- Checking list membership: 'elem' and 'notElem'

hasA xs = 'a' `elem` xs

-- Function null returns True if the list is empty, otherwise false

isEmpty = null l27 -- Use this


-- Exercise 1 --

-- 1.1.
-- Get without first 3 and last 3
getMiddleWithoutOuter3 x = drop 3 $ take (length x - 3) x

-- 1.2.
-- initials s1 s2

initials s1 s2 = take 1 s1 ++ ". " ++ take 1 s2 ++ "."

-- List comprehensions

doubles = [x*2 | x <- [1..10]]

sums1 = [x + y | x <- [1..10], y <- [1..10]]

sums2 = [x + y | x <- [1..10], y <- [1..10], x < y]

sums3 = [x + y | x <- [1..10], y <- [1..10], x < y, odd x || even y]

-- List of sublist lengths:

lengths xss = [length xs | xs <- xss]

totalLength xss = sum $ lengths xss

-- Combining strings

food = [s1 ++ " " ++ s2 | 
        s1 <- ["cold", "hot", "fresh"],
        s2 <- ["cake", "strudel", "salad"]]

-- Exercise 2 --


-- END of exercise --

ws = words "I think we agree, the past is over." -- unwords opposite

ls = lines "First line \n Second line"

stream = unlines ls

-- Filtering words with initial uppercase letter:

capitalized s = [w | w <- words s, isUpper $ head w]

camelCase s = concat [toUpper (head w) : tail w | w <- words s]

-- Or, more sucessfully, using pattern matchin:

camelCase' s = concat [toUpper h : t | (h:t) <- words s]

-- Exercise 3 --
-- 3.1.
letterCount xss = totalLength [xs | xs <- words xss, length xs > 2]

-- 3.2. Redefine palindrome so its insensitive and works for strings that cointain whitespaces

--isPalindromeGood x = []


-- TUPLES --