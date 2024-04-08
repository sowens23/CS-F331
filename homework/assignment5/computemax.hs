-- computemax.lua  
-- Spencer Baysinger
-- 2024-04-07
--
-- For CS 331 Spring 2024
-- Solution to Assignment 5, Exercise C
-- File is written to demonstrate Haskell understanding
-- Print an explanatory message.
    -- Input a sequence of integers from the user, one on each line, ending with a blank line.
    -- If the list is empty, then print a message indicating this. Otherwise, print the maximum item in the list.
    -- Ask the user whether to repeat the program. If so, start over; if not, exit.

import Control.Monad (unless)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "-- This program will input an arbitrary amount of integers into a list --"
  putStrLn "--      And then it will output the maximum integer in that list.      --"
  putStrLn "-------------------------------------------------------------------------"
  putStrLn ""
  nums <- inputNums
  putStrLn ""
  case nums of
    [] -> putStrLn "(Empty list) No Maximum"
    _  -> putStrLn $ "Maximum: " ++ show (maximum nums)
  putStrLn ""
  askRepeat

askRepeat :: IO ()
askRepeat = do
  putStr "Compute another maximum? (y/n): "
  decision <- getLine
  unless (decision /= "y") $ main

inputNums :: IO [Int]
inputNums = go []
  where
    go acc = do
      putStr "Enter an integer: "
      line <- getLine
      case line of
        "" -> return $ reverse acc
        _ -> case readMaybe line of
          Just num -> go (num : acc)
          Nothing -> putStrLn "Invalid input, please enter an integer." >> go acc