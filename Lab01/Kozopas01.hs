{-# OPTIONS_GHC -Wall #-}
module Kozopas01 where

-- Task 1 -----------------------------------------
one, three :: Integer
one = 1
three = 3

power3 :: [Integer]
power3 = [x^three | x <- [1..]]

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [three^x | x <- [one..]]

-- Task 3 -----------------------------------------
listSum::[Integer] -> Integer
listSum xs = if null xs then 0 else head xs + listSum(tail xs)

sumPower3 :: Integer -> Integer
sumPower3 x = listSum [3^i | i <- [1..x]]

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = if m == 0 then 0 else listSum [m^i | i <- [1..n]]

-- Task 5 -----------------------------------------
listLength :: [Int] -> Int
listLength xs = if null xs then 0 else 1 + listLength(tail xs)

lessInList :: [Int] -> Int -> Int
lessInList xs n = listLength (filter (<n) xs)

lessMe :: [Int] -> [Int]
lessMe xs = map (lessInList xs) xs
 
-- Task 6 -----------------------------------------
countIntList :: [Int] -> Int -> Int
countIntList xs n = listLength (filter (==n) xs)

frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency l@(x:_) = (x, (countIntList l x)) : frequency(filter (/=x) l)

-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if mod n 2 == 0 then div n 2 else 3 * n + 1

-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq n = n : hailSeq(hailstone n)

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1..]]

-- Task 10 ----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq n = head (head (filter isLength allHailSeq))
    where isLength xs = length xs == n 
 