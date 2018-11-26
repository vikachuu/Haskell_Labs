{-# OPTIONS_GHC -Wall #-}
module Kozopas02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- Task 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Task 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert xs v = filter (<v) xs ++ [v] ++ filter (>v) xs

sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs

-- Task 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = [i | (i, x) <- zip [0..] xs, p x]

-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse = reverse . map reverse

-- Task 7  -----------------------------------------
notDigit :: Char -> Bool
notDigit n = not (elem n ['0'..'9'])

noDigits :: String -> String
noDigits = filter notDigit

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood xs x = length (filter (==True) (map ($ x) xs))

-- Task 9 ------------------------------------------
nextRow :: [Integer] -> [Integer]
nextRow xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

trianglePas :: [[Integer]]
trianglePas = iterate nextRow [1]

-- Task 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]