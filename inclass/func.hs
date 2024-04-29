-- func.hs
-- Glenn G. Chappell
-- 2024-02-26
--
-- For CS 331 Spring 2024
-- Code from Feb 26 - Haskell: Functions

module Main where


main = do
    putStrLn ""
    putStrLn "This file contains sample code from February 26, 2024,"
    putStrLn "for the topic \"Haskell: Functions\"."
    putStrLn "It will execute, but it is not intended to do anything"
    putStrLn "useful. See the source."
    putStrLn ""


-- ***** Basic Syntax *****


-- A single-line comment goes from "--" to end-of-line.

{- A multiline comment goes from brace-dash to dash-brace. Anything but
dash-brace can appear in a multiline comment, including newlines. -}

-- Function definition
addem a b = a+b

-- Try:
--   addem 2 3

-- Function types
-- Try:
--   :t addem

-- Pattern matching
slowfibo 0 = 0
slowfibo 1 = 1
slowfibo n = slowfibo (n-1) + slowfibo (n-2)
-- Above, the "0", "1", and "n" that appear as the second item on each
-- line are patterns.

-- Try:
--   slowfibo 6

-- Use "where" to introduce a block (indent!) of local definitions

plus_minus_times a b c d = a_plus_b * c_minus_d  where
    a_plus_b = a + b
    c_minus_d = c - d

-- Try:
--   plus_minus_times 1 2 3 4
--   a_plus_b
-- Above, the first should work, but the second should result in an
-- error; the definition of a_plus_b is *local*.

-- We can nest blocks

twicefactorial n = twice (factorial n)  where
    twice k = two*k  where
        two = one + one  where
            one = 1
    factorial 0 = 1
    factorial curr = curr * factorial prev  where
        prev = curr-1


-- ***** Defining Operators *****


-- Infix operator
a +$+ b = 2*a + b

infixl 6 +$+  -- Left associative, same precedence as +

-- Try:
--   2 +$+ 3
--   2 +$+ 3*5
--   (2 +$+ 3)*5

-- We can use a normal function as an infix operator
-- Try:
--   2 `addem` 3

-- And we can use an operator as a normal function
-- Try:
--   (+$+) 2 3
--   :t (+$+)


-- ***** Currying *****


-- Try:
--   (addem 2) 3

-- Try:
--   addem (2 3)
-- You should get an error.

add2 = addem 2

-- Try:
--   add2 3
--   add2 7
--   :t add2


-- ***** Lambda Functions *****


-- Two ways to define a function
square x = x*x

square' = \ x -> x*x  -- \ x -> x*x is an unnamed function
                      --  ("lambda function" or "lambda expression")

-- Try:
--   square' 5
--   (\ x -> x*x) 5

-- The following do the same computation as function addem


addem' = \ a b -> a+b

addem'' = \ a -> (\ b -> a+b)

addem''' a = \ b -> a+b

-- Try:
--   addem' 2 3
-- (or any of the other versions)

