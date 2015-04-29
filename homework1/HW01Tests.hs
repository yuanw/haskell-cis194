-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (a, xs) = toRevDigits a == xs

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits test" testToRevDigits
            [(123, [3, 2, 1]), (0, []), (-17, [])]]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (a, b) = doubleEveryOther a == b
ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther
            [([0, 0] ,  [0, 0]), ([4, 9, 5, 5], [4, 18, 5, 10])]]

-- Exercise 4 -----------------------------------------
testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, a) = sumDigits xs == a

ex4Tests :: [Test]
ex4Tests =  [Test "sumDigits test" testSumDigits
            [([0, 0] , 0),
             ([10, 5, 18, 4], 19)]]

-- Exercise 5 -----------------------------------------

testluhn :: (Integer, Bool) -> Bool
testluhn (a, result) = luhn a == result

ex5Tests :: [Test]
ex5Tests = [Test "Test luhn" testluhn
            [(4916407038322014, True),
             (1234567898765432, False),
             (5594589764218858, True),
             (5230976158938045, True)]]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
