\ adv_flow.fs
\ Glenn G. Chappell
\ 2024-03-25
\
\ For CS 331 Spring 2024
\ Code from Mar 25 - Forth: Advanced Flow


cr cr
." This file contains sample code from March 25, 2024," cr
." for the topic 'Forth: Advanced Flow'." cr
." It will execute, but it is not intended to do anything" cr
." useful. See the source." cr
cr


\ ***** Background *****


\ Some Forth words have access to the code that comes after them. These
\ are called *parsing words*. Examples of parsing words that we have
\ seen include "see" and "to".

\ hello
\ Simple example word. Prints newline, a greeting, and another newline.
: hello  ( -- )
  cr
  ." Hello there!" cr
;

\ Try:
\   see hello


\ ***** Execution Tokens *****


\ In Forth, a *token* is an integer that represents some component.

\ An *execution token* is a number that represents the code for some
\ executable word. When typing code interactively, get the execution
\ token for an executable word using the parsing word ' (single quote);
\ the next word should be the word whose execution token you want.

\ Try:
\   ' hello .
\   ' dup .
\   ' hello .

\ In a compiled context, ' should be replaced by ['] (single quote in
\ brackets).

\ print-hello-xt
\ Prints execution token for word "hello".
: print-hello-xt  ( -- )
  cr
  ['] hello . cr
;

\ Try:
\   print-hello-xt
\   ' hello .

\ The word "execute" takes an execution token, which it pops off the
\ data stack. It then executes the corresponding code.

\ Try:
\ 5 7 ' + execute .

\ This lets us pass a word to another word. Below is a word that takes
\ an execution token and calls the corresponding code with certain
\ parameters.

\ apply-to-5-7
\ Takes an execution token, which is popped. Executes the corresponding
\ code with 5 7 on the data stack. The "in1 ... inm" in the stack effect
\ below represent any other parameters the code to be executed might
\ have. The "out1 ... outn" are its results.
: apply-to-5-7  { xt -- ?? }  \ MAYBE
  5 7 xt execute
;

\ Try:
\   ' + apply-to-5-7 .
\   ' * apply-to-5-7 .

\ Now we can write "map", as we did in Haskell. We reuse words intsize,
\ alloc-array, print-array, and set-array from alloc.fs. As before,
\ sizei is the number of items in an integer array, not the number of
\ bytes required.

\ intsize
\ Assumed size of integer (bytes)
: intsize 8 ;

\ alloc-array
\ Allocates an array holding the given number of integers. Throws
\ exception on allocation fail.
: alloc-array  { sizei -- addr }
  sizei intsize * allocate throw { addr }
  addr      \ Return our array
;

\ print-array
\ Prints items in given integer array, all on one line, blank-separated,
\ ending with newline.
: print-array  { addr sizei -- }
  sizei 0 ?do
      i intsize * addr + @ .
  loop
  cr
;

\ set-array
\ Sets values in given array. Array starts at addr and holds sizei
\ integers. Item 0 is set to start, item 1 to start+step, item 2 to
\ start+2*step, etc.
: set-array  { start step addr sizei -- }
  start { val }
  sizei 0 ?do
    val i intsize * addr + !
    val step + to val
  loop
;

\ map-array
\ Given an integer array, represented as pointer + number of items, and
\ an execution token for code whose effect is of the form ( a -- b ).
\ Does an in-place map, using the execution token as a function. That
\ is, for each item in the array, passes the item to this function,
\ replacing the array item with the result. Throws on bad array size.
: map-array { arr sizei xt -- }
  sizei 0 < throw  \ Throw on negative array size
  0 { loc }        \ loc: ptr to array item, used like a C++ iterator
  sizei 0 ?do
    arr i intsize * + to loc  \ Get pointer to current array item
    loc @ xt execute loc !    \ Map current array item
                              \  Similar to C++: *loc = xt(*loc);
  loop
;

\ square
\ Returns the square of its parameter. Example for use with map-array.
: square  { x -- x**2 }
  x x *
;

\ do-map
\ Creates an integer array holding the given number of items, filling it
\ with data, and calls map-array, passing "square", on this array. The
\ array items are printed before and after the map. Throws on failed
\ allocate/deallocate.
: do-map  { sizei -- }
  sizei alloc-array { arr }
  1 1 arr sizei set-array  \ Fill array: 1, 2, 3, ...
  cr
  ." Doing map-array with square" cr
  ." Values before map: " arr sizei print-array
  arr sizei ['] square map-array
  ." Values after map: " arr sizei print-array
  arr free throw
;

\ Try:
\   18 do-map


\ ***** Exceptions *****


\ Forth has exception handling. This works similarly to other PLs with
\ exceptions (C++, Python). When an exception is thrown, the currently
\ executing code is exited. If a handler is found, then this is
\ executed; otherwise, the program crashes.
\
\ However, since Forth has neither type checking nor an extensible type
\ system, different kinds of exceptions are not distinguished by type,
\ as they are in many other PLs. Instead the kind of exception is given
\ by an integer. Zero means "no exception". Values -255 through -1 are
\ reserved for certain predefined exceptions (on my version of Gforth,
\ -58 through -1 are used, with -255 through -59 being reserved for
\ future use). Values -4095 through -256 are assigned by the system. So
\ if you want to use your own exception numbers, these should be either
\ positive or less than -4095.

\ ** Custom Messages **

\ To assocate an exception code to a given error-message string, pass
\ the string (address & length) to "exception". The return value is the
\ exception code, which will be in the range -4095 .. -256. Successive
\ calls to "exception" will return different codes.
\ exception  ( str-addr str-len -- exception-code )

\ Try:
\   s" A bad thing happened" exception throw
\ Note the error-message string that is printed.

\ Here is a word that may throw, using "exception" if it does.

\ mysqrt
\ Given an integer, returns the floor of its square root. Throws on a
\ negative parameter.
: mysqrt  { x -- sqrt(x) }
  x 0 < if
    s" mysqrt: parameter is negative" exception throw
  endif
  0 { k }
  begin
    k k * x <= while
    k 1 + to k
  repeat
  k 1 -
;

\ Try:
\   40 mysqrt .
\   -40 mysqrt .

\ ** catch **

\ Use "catch" to catch an exception that might be thrown by some word.
\ As in other PLs, this includes exceptions thrown by other words that
\ it calls, if those exceptions have not been caught inside the word.
\
\ "catch" is similar to "execute": it takes an execution token, and then
\ it executes the code. However, it also catches exceptions. After the
\ word is executed:
\ - If no exception was thrown: push 0.
\ - If an exception was thrown: the stack has the same depth as before
\   "catch" was called. The top item (previously the execution token) is
\   replaced by the exception code. Any other items that were popped off
\   by execution are replaced by arbitrary data. Items lower on the
\   stack remain unchanged.
\
\ Note that, after "catch", the top of the stack is 0 if no exception
\ was thrown; it is the exception code (which is nonzero) if an
\ exception was thrown. So we can test whether an exception was thrown
\ with "if".
\
\ Exception-handling code is allowed to re-throw the exception, or a
\ different kind of exception (there is no special mechanism for this;
\ pass an exception code to "throw", as usual). If this is *not* done,
\ then exception-handling code should call "nothrow", which resets the
\ information stored about the exception, so it does not get mixed up
\ with that for later exceptions.
\ nothrow  ( -- )

\ call-mysqrt
\ Pass the given value to "mysqrt", catching any exception thrown and
\ printing the results (exception or not).
: call-mysqrt  { x -- }
  cr cr
  ." Passing " x . ." to mysqrt" cr
  x ['] mysqrt catch { exception-code }
  exception-code if
    \ An exception was thrown
    drop     \ This pops the value that is on the stack to make the
             \ stack depth after "catch" is the same as it was before
             \ "catch".
    ." Exception thrown; code: " exception-code . cr
    nothrow  \ Reset error handling (does not affect the stack)
             \ Do this if we do NOT re-throw
  else
    \ No exception was thrown
    { result }
    ." No exception thrown" cr
    ." Result: " result . cr
  endif
  cr
;

\ Try:
\   40 call-mysqrt
\   -40 call-mysqrt

\ Here is much the same idea as call-mysqrt, except that, if mysqrt
\ throws an exception, then we re-throw the same kind of exception.

\ call-mysqrt2
\ Pass the given value to "mysqrt", catching any exception thrown and
\ printing the results (exception or not). If an exception is thrown,
\ then we re-throw the same kind of exception.
: call-mysqrt2  { x -- }
  cr cr
  ." Passing " x . ." to mysqrt" cr
  x ['] mysqrt catch { exception-code }
  exception-code if
    \ An exception was thrown
    drop     \ This pops the value that is on the stack to make the
             \ stack depth after "catch" is the same as it was before
             \ "catch".
    ." Exception thrown; code: " exception-code . cr
    exception-code throw
             \ Re-throw the exception
  else
    \ No exception was thrown
    { result }
    ." No exception thrown" cr
    ." Result: " result . cr
  endif
  cr
;

\ Try:
\   40 call-mysqrt2
\   -40 call-mysqrt2

