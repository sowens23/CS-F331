\ collcount.fs
\ Spencer Baysinger
\ 2024-04-29

\ For CS 331 Spring 2024

\ Assignment 7 Exercise A - Running a Nilgai Program
  \ Secret message: The Magic Words are Squeamish Ossifrage

\ Assignment 7 - Exercise B: Programming in Forth
  \ Write a Forth source file collcount.fs as follows. Be sure to follow the Coding Standards.
  \ Your file should define the word collcount.
  \ Word collcount should have stack effect (n -- c), where n is a positive integer, and c is the number of iterations of the Collatz function (see Assignment 5) required to take n to 1.
  \ Your code does not need to do any type checking or other error checking

  \ Note to self: Run using gforth on a unix machine

: collcount ( n -- c )
  0 swap
  begin
    dup 1 >
  while
    dup 2 mod if
      3 * 1+
    else
      2 /
    then
    swap 1+ swap
  repeat
  drop ;