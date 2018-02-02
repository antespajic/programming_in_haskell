module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
-- Function finds local maximums of a list,
-- elements of the list which is strictly greater than both the elements immediately before and after it
localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (_:_:[]) = []
localMaxima (x:y:z:rest)
  | y > x && y > z = y : localMaxima (y:z:rest)
  | otherwise = localMaxima (y:z:rest)

-- Task 02
-- Extract the scrabble scores from a legacy system and convert them to the new system
-- From [(1, "ABC"), (2, "DEF")] to [('a', 1), ('b', 1), ('c', 1), ('d', 2), ('e', 2), ('f', 2)]
transform :: [(Int, String)] -> [(Char, Int)]
transform [] = []
transform ((value, (c :[])) :rest) = (toLower c, value) :transform rest
transform ((value, (c :str)) :rest) = (toLower c, value) :transform ((value, str):rest)

-- Task 03
-- The cellular automaton consists of a one-dimensional array of cells which can be either on or off.
-- Each code describes a set of rules which determines the next configuration of cells based on their neighbours.
-- simplified version of the automaton, which works on a fixed-size array, and only implements the simple Rule 90
-- https://en.wikipedia.org/wiki/Rule_90
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : (rule90 $ False :rule90Step xs)

rule90Step :: [Bool] -> [Bool]
rule90Step [] = []
rule90Step (_:_:[]) = False: []
rule90Step (x:y:z:rest) = x `xor` z : rule90Step (y:z:rest)

pretty :: [[Bool]] -> String
pretty (xs:[]) = printArr xs
pretty (xs:xss) = printArr xs ++ pretty xss

printArr :: [Bool] -> String
printArr [] = '\n' : []
printArr (False:xs)= ' ' :printArr xs
printArr (True:xs) = '#' :printArr xs  

-- Task 04
f :: [String]
f = undefined
