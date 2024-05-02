#lang scheme

; addpairs.scm
; Spencer Baysinger
; 2024-04-29

; For CS 331 Spring 2024

; Assignment 7 - Exercise C
  ; Write a Scheme source file addpairs.scm as follows. Be sure to follow the Coding Standards.
  ; Your file should begin with the line “#lang scheme”.
  ; Your file should define procedure addpairs.
  ; This takes an arbitrary number of arguments, which should be numbers. 
  ; It returns a list consisting of the sum of the first two numbers, then the sum of the next two numbers.
  ; etc. If there are an odd number of arguments, then the last item in the returned list is the last argument. 
  ; For example evaluating (addpairs 1 2 33 44) should return (3 77), because , and . And (addpairs 1 2 33) should return (3 33).
  ; Your code does not need to do any type checking or other error checking.

; Note to self: Run using DrRacket.exe

(define (addpairs . args)
  (if (null? args)
    '()
    (let ((first (car args))
        (rest (cdr args)))
      (if (null? rest)
        (list first)
        (let ((second (car rest))
              (remaining (cdr rest)))
          (cons (+ first second) (apply addpairs remaining))
        )
      )
    )
  )
)
