{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x: xs) (x': xs') = (if x == x' then 1 else 0) + (exactMatches xs xs')

-- Exercise 2 -----------------------------------------
append :: [Int] -> [Int] -> [Int]
append [] _ = []
append _ [] = []
append (x: xs) (x' : xs') = (x + x') : (append xs xs')


-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = [0, 0, 0, 0, 0, 0]
countColors (x: xs') = [if x == color then 1 else 0 | color <- colors] `append` (countColors xs')

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
-- matches a b = listEqual (countColors a) (countColors b)
matches x y = sum([if (a /= 0) && (b /= 0) then 1 else 0 | (a, b) <- zip (countColors x) (countColors y)])

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b (exactMatches a b) (matches a b)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move a exactM m) b = (exactM == (exactMatches b a)) && (m == (matches b a))

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m codes = [code <- codes, isConsistent m c]
-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
