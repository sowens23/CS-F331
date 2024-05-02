% collcount.pl
% Spencer Baysinger
% 2024-04-29

% For CS 331 Spring 2024

% Assignment 7 - Exercise D
%   Write a Prolog source file collcount.pl as follows. Be sure to follow the Coding Standards.
%   Your file should define the predicate collcount/2; that is, collcount is a predicate that takes 2 arguments.
%   Predicate collcount should take arguments as follows: collcount(+n, ?c); that is, the first argument is an input, while the second is either an input or an output.
%   The idea is that n is a positive integer, and c is the number of iterations of the Collatz function required to take n to 1 (just as in the Forth exercise).
%   Your code does not need to do any type checking or other error checking.

% Note to self: Open SWI-PROLOG "swipl-win.exe" in the folder, and then type "['collcount.pl]." to load, then operate functions.

collcount(1, 0).
collcount(N, C) :-
    N > 1,
    (
        0 is N mod 2 ->
        M is N // 2
    ;
        M is N * 3 + 1
    ),
    collcount(M, C1),
    C is C1 + 1.