import Data.Char
import Data.List

x = 2

-- Single argument function
inc x = x + 1

-- Two argument function
digits2Number x y = x * 10 + y

y = inc 2
z = digits2Number 4 2

name = "Humpty Dumpty"

letter = 'H'

--Concatenating strings

s = "One " ++ "two " ++ "three "

n1 = length "The quick brown fox jumps over the lazy dog"
n2 = length s

-- If then else

condDec x = if x > 0 
    then x - 1 
    else x

foo x = (if even x then x*2 else 2) + 1
-- Not the same as
foo' x = if even x then x*2 else 2 + 1

bigNumber x = if x>= 1000 then True else False
-- Avoid explicitly returning True/False; instead just return the expression

bigNumber' x = x >= 1000

-- Playing with strings a bit

merge s1 s2 = 
    s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

merge2 s1 s2 =
    s1 ++ " is " ++ (if s1 < s2 then " not " else "") ++ s2

-- Guards

merge3 s1 s2
  | s1 < s2 = s1 ++ " is not " ++ s2
  | otherwise = s1 ++ " is " ++ s2

grade score
  | score < 50 = 1
  | score < 63 = 2
  | score < 76 = 3
  | score < 89 = 4
  | otherwise = 5

showSalary amount bonus
  | bonus /= 0 = "Salary is " ++ show amount ++ " , and a bouns " ++ show bonus
  | otherwise = "Salary is " ++ show amount

-- Exercise 1 
-- 1.1
concat3 str1 str2 str3
  | length str2 < 2 = str1 ++ str3
  | otherwise = str1 ++ str2 ++ str3

-- Lists

l1 = [1, 2, 3]

-- Operator ':' (so-called "cons")

l1' = (1:(2:(3:[])))
l1'' = 1:2:3:[]

-- List concatenation

l2 = [1,2,3] ++ [4,5,6]

myConcat l1 l2 = l1 ++ l2

-- Turning elements into singleton lists:

listify x = [x]
listify' x = x:[]

-- Extracting parts of a list: head, tail, init, last.

l3 = take 3 [9,2,10,3,4]
l4 = drop 3 [9,2,10,3,4]

-- Reversing a list 

l5 = reverse [1,2,3]

-- Strings are lists of characters

l6 = "this is a list"
l7 = head l6

l8 = 'H' : "askell"

-- Is a string a palindrome?

isPalindrome s = s == reverse s
