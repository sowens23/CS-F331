-- io.hs
-- Glenn G. Chappell
-- Started: 2024-03-01
-- Updated: 2024-03-04
--
-- For CS 331 Spring 2024
-- Code from Mar 1 & 4 - Haskell: I/O

module Main where

import System.IO  -- for hFlush, stdout


main = do
    putStrLn ""
    putStrLn "This file contains sample code from March 1 & 4, 2024,"
    putStrLn "for the topic \"Haskell: I/O\"."
    putStrLn "It will execute, but it is not intended to do anything"
    putStrLn "useful. See the source."
    putStrLn ""


-- ***** String Conversion *****


-- Function "show" converts anything showable (type must be in class
-- Show) to a String.

-- numConcat
-- Returns string containing 2 params separated by blank.
-- So (numConcat 42 7) returns "42 7".
numConcat a b = (show a) ++ " " ++ (show b)

-- Try:
--   numConcat 42 7


-- Function "read" converts a string to anything (type must be in class
-- Read).

-- Try:
--   read "42"
-- Result is error; need to force return type.


stringPlusOne str = 1 + read str

stringToInteger str = (read str)::Integer

-- Try:
--   stringPlusOne "42"
--   stringToInteger "42"


-- ***** Simple Output *****


-- An I/O action is type of value. We do I/O by returning an I/O action
-- to the outside world.

sayHowdy = putStr "Howdy!"

sayHowdyNewLine = putStrLn "Howdy!"

-- Use ">>" to join I/O actions together into a single I/O action

sayHowdyAgain = putStr "Howdy " >> putStrLn "thar!"

sayHowdy2Line = putStrLn "Howdy" >> putStrLn "thar!"

-- Try:
--   sayHowdy
--   sayHowdyNewLine
--   sayHowdyAgain
--   sayHowdy2Line

secondsInADay = 60*60*24

ss1 = "There are "
ss2 = show secondsInADay
ss3 = " seconds in a day."
printSec = putStrLn (ss1 ++ ss2 ++ ss3)

io1 = putStr "There are "
io2 = putStr (show secondsInADay)
io3 = putStrLn " seconds in a day -- or so they say."
printSec' = io1 >> io2 >> io3

-- Try:
--   printSec
--   printSec'


-- ***** Simple Input *****


-- An I/O action wraps a value. The above I/O actions all wrapped
-- "nothing" values. getLine returns an I/O action that wraps the String
-- that is input.

-- Send the wrapped value to a function with ">>="

getPrint   = getLine >>= putStrLn
getPrint'  = getLine >>= (\ line -> putStrLn line)

rMsg = "What you typed, reversed: "
reverseIt' = putStr "Type something: "
             >> getLine
             >>= (\ line -> putStrLn (rMsg ++ reverse line))

-- The wrapped value cannot be removed from the I/O action, but it can
-- be processed inside it (e.g., the above call to function reverse).

-- Try:
--   getPrint
--   getPrint'
--   reverseIt'


-- ***** Do-Expression *****


-- The do-expression is simple syntactic sugar around the ">>" and
-- ">>=" operators.

-- inputLength
-- Input a line from the user, and print a message giving its length.
inputLength = do
    putStr "Type some text: "
    hFlush stdout      -- Make sure prompt comes before input
    line <- getLine
    putStrLn ""
    putStr "You typed: "
    putStrLn line
    putStr "Length of your line = "
    putStrLn $ show $ length line

-- Try:
--   inputLength

-- Note that, inside an I/O do-expression, NAME <- IO_ACTION binds NAME
-- to the value wrapped by IO_ACTION.


-- ***** return *****


-- Inside an I/O do-expression, "return" creates a do-nothing I/O action
-- wrapping a value of our choice. It does NOT return.
--
--     return x
-- gives a no-side-effect I/O action wrapping the value x.
--
--     return ()
-- gives a no-side-effect I/O action wrapping a "nothing" value.

-- myGetLine
-- Same as getLine, but showing how to write it.
-- Uses "return".
myGetLine = do
    c <- getChar  -- getChar does what you think;
                  --  return value is I/O-wrapped Char
    if c == '\n'
    then return ""
    else do
        rest <- myGetLine
        return (c:rest)

-- Note: Expressions in an I/O do-expression need to return I/O actions,
-- but they can be complicated expressions, like if-then-else above.

-- inputLength'
-- Same as inputLength, but rewritten to use myGetLine.
inputLength' = do
    putStr "Type some text: "
    hFlush stdout      -- Make sure prompt comes before input
    line <- myGetLine  -- Use our version of getLine
    putStrLn ""
    putStr "You typed this nonsense: "
    putStrLn line
    putStr "Length of your line = "
    putStrLn $ show $ length line

-- Try:
--   inputLength'


-- ***** "let" In a Do-Expression *****


-- Final bit of do-expression syntax:
--   let NAME = EXPRESSION
-- binds a name to a NON-I/O value, for remainder of do-expression.

-- squareNums
-- Repeatedly input a number from the user. If 0, then quit; otherwise
-- print its square, and repeat.
squareNums = do
    putStr "Type an integer (0 to quit): "
    hFlush stdout       -- Make sure prompt comes before input
    line <- getLine     -- Bind name to I/O-wrapped value
    let n = read line   -- Bind name to non-I/O value
                        -- Compiler knows n is Integer by how we use it
    if n == 0
    then return ()  -- Must have I/O action here, but there are no side
                    --  effects to perform and nothing it needs to wrap.
    else do
        putStrLn ""
        putStr "The square of your number is: "
        putStrLn $ show $ n*n
        putStrLn ""
        squareNums    -- Repeat

-- Try:
--   squareNums

-- Also see file squarenums.hs

