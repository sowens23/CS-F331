-- PA5.hs
-- Spencer Baysinger
-- 2024-04-05

-- For CS 331 Spring 2024

-- Assignment 5, part A - Haskell Variables & Functions
-- Secret message #4:
-- Every earless elephant eagerly eats endive early each evening.

-- Assignment 5, part B - Haskell Variables & Functions
-- This assignment we will demonstrate Haskell knowledge by creating a module 
-- containing functionality to deal with lists and numbers.

-- File based on PA5.hs (skeleton) written by Professor Glenn Chappell
        -- PA5.hs  SKELETON
        -- Glenn G. Chappell
        -- 2024-03-19
        --
        -- For CS 331 Spring 2024
        -- Solutions to Assignment 5 Exercise B

module PA5 where

import Data.List (isPrefixOf, tails, findIndex)
-- =====================================================================

-- collatzCounts

collatzCounts :: [Integer]
collatzCounts = map collatz [1..]

collatz :: Integer -> Integer
collatz 1 = 0
collatz n
    | even n    = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (3*n + 1)

-- =====================================================================

-- filter2
filter2 :: (a -> Bool) -> [a] -> [b] -> [b]
filter2 p xs ys = [y | (x, y) <- zip xs ys, p x]

-- =====================================================================

-- operator <#
(<#) :: Ord a => [a] -> [a] -> Int
xs <# ys = length [1 | (x, y) <- zip xs ys, x < y]

-- =====================================================================

-- listSearch
listSearch :: Eq a => [a] -> [a] -> Maybe Int
listSearch [] _ = Just 0
listSearch _ [] = Nothing
listSearch xs ys
    | length xs > length ys = Nothing
    | otherwise = findIndex (isPrefixOf xs) (tails ys)


-- =====================================================================


-- concatEvenOdd
concatEvenOdd :: [String] -> (String, String)
{-
  The assignment requires concatEvenOdd to be written as a fold.
  Like this:

    concatEvenOdd xs = fold* ... xs  where
        ...

  Above, "..." should be replaced by other code. "fold*" must be one of
  the following: foldl, foldr, foldl1, foldr1.
-}
concatEvenOdd xs = foldl f ("", "") (zip [0..] xs)
  where
    f (evens, odds) (index, value)
      | even index = (evens ++ value, odds)
      | otherwise  = (evens, odds ++ value)


