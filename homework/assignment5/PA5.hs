-- Secret message #4:
-- Every earless elephant eagerly eats endive early each evening.

-- PA5.hs
-- Spencer Baysinger
-- 2024-04-05
--
-- For CS 331 Spring 2024
-- Assignment 5, part B - Haskell Variables & Functions
-- This assignment we will demonstrate Haskell knowledge by creating a module containnig functionality to deal with lists and numbers.

-- File based on PA5.hs (skeleton) written by Professor Glenn Chappell
        -- PA5.hs  SKELETON
        -- Glenn G. Chappell
        -- 2024-03-19
        --
        -- For CS 331 Spring 2024
        -- Solutions to Assignment 5 Exercise B

module PA5 where


-- =====================================================================


-- collatzCounts
collatzCounts :: [Integer]
collatzCounts = [42..]  -- DUMMY; REWRITE THIS!!!


-- =====================================================================


-- operator <#
(<#) :: Ord a => [a] -> [a] -> Int
_ <# _ = 42  -- DUMMY; REWRITE THIS!!!


-- =====================================================================


-- filter2
filter2 :: (a -> Bool) -> [a] -> [b] -> [b]
filter2 _ _ bs = bs  -- DUMMY; REWRITE THIS!!!


-- =====================================================================


-- listSearch
listSearch :: Eq a => [a] -> [a] -> Maybe Int
listSearch _ _ = Just 42  -- DUMMY; REWRITE THIS!!!


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
concatEvenOdd _ = ("Yo", "Yoyo")  -- DUMMY; REWRITE THIS!!!

