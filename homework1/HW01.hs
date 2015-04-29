{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit a = a `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit a = read $ "0" ++ (init $ (show a))

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits a
    | a <= 0    = []
    | otherwise =  lastDigit a : (toRevDigits (dropLastDigit a))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a = [ if (snd i) `mod` 2 == 0 then 2 * fst(i) else fst i  | i <- zip a ([1, 2..] :: [Int])]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr (\ x y -> y + (sum $ toDigits x) ) 0


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
