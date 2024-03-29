# CS-F331 Computer Languages
## Class Notes and Homework Repository :dizzy: :earth_americas:
  ```
  #include <iostream>
  int main() {
    std::cout << "Hello traveller, here's a towel for protection. Take care of yourself out there." << std::endl;
    return 0;
  }
  ```
  ![Hitchhikers Guide to the Galaxy](https://github.com/sowens23/CS-F311/blob/main/inclasscoding/week1/tenor.gif)

### Class Repositories and References
  - [sowens23-GitHub](https://github.com/sowens23)
  - [GitHubPortal](https://github.com/sowens23/Newbie-Gains/blob/main/README.md)
  - [CS-F331 Class Homepage](https://www.cs.uaf.edu/~chappell/class/2024_spr/cs331/)
  - [CPP Reference Useful Resources](https://en.cppreference.com/w/cpp/links)

### Big Notes
  - 

# Class Notes and Assignments
  | Weekly Notes | Assignments | Other Notes |
  | --- | --- | --- |
  | [Week-1](#Week-1) | | |
  | [Week-2](#Week-2) | | |
  | [Week-3](#Week-3) | | |
  | [Week-4](#Week-4) | | |
  | [Week-5](#Week-5) | | |
  | [Week-6](#Week-6) | | |
  | [Week-7](#Week-7) | | |
  | [Week-8](#Week-8) | | |
  | [Week-9](#Week-9) | | |
  | [Week-10](#Week-10) | | |
  | [Week-11](#Week-11) | | |
  | [Week-12](#Week-12) | | |
  | [Week-13](#Week-13) | | |
  | [Week-13](#Week-13) | | |
  | [Week-14](#Week-14) | | |

# Week-9
[Top](#TOP)
## 2024-03-29
  ### Specifying Semantics
  - Semantics is used in the following ways:
    1. Compiler: Needs to be designed to be based on the semantics of both source PL and target PL.
    2. Interpreter: Needs to be designed to be based on the semantics of the source PL.
    3. Optimization: Altering codeto improve performance while keeping its semantics the same.
    4. Verification: Using semantics to check that code performs the correct actions.
  - **Specification** for a PL should provide enough information that a compiler and could be developed from it.
  - A **formal specification method** is a mathematically based technique for describing something.
    - Other methods are **informal**.
  - **Formal semantics** refers to formal specification methods for semantics
  - **Operational semantics** specify semantics of a PL in terms of the semantics of some other PL or abstract machine.
  - **Denotational semantics** specify semantics by representing state & values with mathematical objects, commands, & computations by functions.
  ### How Interpreters Work
  - A **compiler** takes code in on PL (source) and translates it into code in another PL (target)
  - An **interpreter** takes code in it's source PL and executes it.
  - Compilation and interpretation are not mutually exclusive. Sometimes an interpreter contains a compiler, and sometimes there may be an **intermediate representation (IR)** that translates code into a lower-level code like byte code, before it's interpreted.
  - Code modules that does that actual execution usually fall in one of four categories:
    1. **Text-Based Interpreter**: This interpreter goes through the high-level source code and executes it directly, wit little prerocessing, or IR being used. These interpreters generally offer poor performance compared to other methods. 
    2. **Tree-Walk Interpreter**: If we have parsed our source code into an Abstract Syntax Tree, then we can use a tree-walk interpreter, executing as it traverses the abstract tree.
    3. **Virtual Machine**: The fastest way to execute code is to parse the source code into an Abstract Syntax Tree and compile it into a **byte code**, which then executes the functions at a low machine-code level. This is the most common kind of interpreter used today. This is the standard for Lua, Python, and any number of other PLs.
    4. **JIT**: The fastest interpreter method is **Just-In-Time (JIT)**. This style of interpreter executes code compiled into byte code and is executed immediately as it's compiled into machine code. This is becoming increasingly common, and is used in **LuaJIT** and **PyPy**, and is more labor intensive.
      - JIT divides up code to be executed into sections. Ideally, sections involve little flow of control. The byte code from sections are executed using a VM. Between section execution, control returns to the main JIT code.
      - JIT actually stands for compiling at *optimal* time.
      - Later in compilation, you can use information from code execution, for example **profile-based optimization** based on what portions of the code spends the most time doing.
      - Each code section is rated **hot** to **cold** indication the priority of fast execution.
        - Hot code is more likely to be compiled, aggressively optimized, and re-compiled with more optimization
        - Code sections are rated by how many times they are executed, how fast they can be executed, and priority of fast execution is for that section.
      - JIT was invented by Xerox's Palo Alto Research Center (PARC) for PL **Self**.
  
## 2024-03-27
  ### Stuff
  - Not too much done, we just spent most of our time working on a file.
## 2024-03-25
  ### Forth: Advanced Flow
  -  **Open-Closed Principle** is when code should be open for extension, but closed for modificaitons.
  - **Dependency Injection** is when you pass code to a module for that module to execute.
    - This is a way to add capabilities to code without modifying the code.
    - ANS Forth has no support for OOP, parametrized types, and first-class functions. But it allows for dependency injection through the use of *execution tokens*.
  - A Forth **parsing word** is a word that has access to the code that comes after it. We use parsing words primarily when we want to do something with a word that does not involve immediately executing it.
  - In Forth, a **token** is an integer that represents some component. 
  - An **execution token** is an integer that represents the code for an executable word. Given a word's execution token, we can call the word.

# Week-8
[Top](#TOP)
## 2024-03-22
  ### Review
  - **Stack-effect** notation is a Forth *convention* showing the effects of a word on the stack.
    - WORD ( ... -- ... )  :
      - Before and after of the stack before and after WORD is called
        - + ( a b -- a + b)
        - dup (a -- a a)
    - { a b -- a + b} : 
      - "a b" creates two local variables, sets them to first two popped values from stack
      - "a + b" is just a comment
  ### Forth: Allocation & Arrays
  - **Fetch** (**@**) takes a memory adress and pushes the integer valuefound at that address: @ ( value addr -- )
  - **Store** (**!**)  takes an integer value and a memory address. It stores the value at the address: ! (value addr -- )
  - Forth's **dictionary** is an internal data structure holding defined words, in the order they were defined.
  - **Exception Codes** are exceptions distinguished by integer
    - 0 means "no exception"
    - -4095 through -1 are assigned by the system. Some of these may be associated with error messages
    - Positive values and values less then -4095 are available for arbitrary uses by programmers
  - let's write a word that takes an input string and writes it backwards
    ```fs
    \ backtype - given a string (addr, len) print it backward.
    : backtype  { addr len -- }
      0 { k }
      len 0 ?do
        len 1 - i - to k \ k counts from len-1 down to 0
        addr k + c@ { thechar }
        thechar emit
      loop
    ;

    : printrev ( -- )
      100 { buf-size }
      buf-size allocate throw { buf-addr }
      cr cr
      ." Type something: "
      buf-addr buf-size accept { input-len }
      cr cr
      ." Here is what you typed: "
      buf-addr input-len type
      cr cr
      ." And here it is backwards: "
      buf-addr input-len backtype
      cr cr
      buf-addr free throw
    ;
    ```
  - Chappell's quote of the day "You may be uninformed, but atleast you're all entertaining, and that's worth something!"

## 2024-03-20
  ### Thoughts on Assignment 5
  - Three excercises
    1. Run some code in Scheme
      - Run *check_scheme.scm* in **DrRacket IDE**
      - DrRacket expects the first time to indicate the PL
        - #lang scheme
      - You may need to delete this if you don't use DrRacket
    2. Write a module in Haskell containing five things
      - Using a skeleton file, write PA5.hs
      - Be sure to write a function *concatEvenOdd*, it is required to be written as a **fold**
        ```scm
        concatEvenOdd xs = fold* ... xs  where 
        ```
      - consider code ``` foldl f z xs ``` where;
        - xs is the list we are operating on
        - z is the starting value, it is what is returned if xs is empty
        - f is a 2-parameter function that takes a partial result and a new item, it returns the result we want for the whole list
        - For example ``` foldl (+) 0 [3, 6, 2, 5, 3, 7] ``` returns the sum of all list items.
      - foldr is the same as foldl, except that it works in the other direction, fold left and right.
    3. Write a simple stand-along program in Haskell
      - Your code will need to do some I/O tasks repeatedly, many times.
      - Writing a function that takes I/O then calls itself tail recursively is an easy way to do this.
      - See *squarenums.hs* for an example of how this may work.
      - Your program does not need to be **robust**, it is allowed to crash if wrong types are input.
  ### Forth: Words
  - Concatenative PLs include **Forth**, **PostScript**, and **Factor**.
  - List most concatenative PLs, Forth is **stack-based**.
  - Forth has four stacks.
    - Data Stack: "." for integers
    - Floating Point Stack: "f." for floating point values
    - Local Variable Stack: ?. for local variables?
    - Call Stack: ?. for calls?
## 2024-03-18
  ### Where Are We?
  - Upon successful completion of CS 331, students should:
    - Understand the concepts of syntax and semantics, and how syntax can be specified.
    - Understand, and have experience implementing, basic lexical analysis, parsing, and interpretation.
    - Understand the various kinds of programming languages and the primary ways in which they differ.
    - Understand standard programming language features and the forms these take in different programming languages.
    - Be familiar with the impact (local, global, etc.) that choice of programming language has on programmers and users.
    - Have a basic programming proficiency in multiple significantly different programming languages.
  ### PL Category: Concatenative PLs
  - A concatenative programming language is one in which the concatenation of two programs is a valid program, with the data returned by the first part being passed to the second part.
    - The first major concatenative language was Forth, developed by Charles H. Moore beginning in the 1960s
  - **Postscript**, from Adobe Systems is the most heavily used concatenative language used today.
  - A typical concatenative language has the following characteristics.
    - It is **stack-based**: stacks are the primary means of passing around data.
    - Usually syntax is simple, consisting of **words**, seperated by whitespace.
    - It is **extensible**: built-in constructs have the same status as those put together by a programmer.
  ### Intro to Forth 
  - Forth has a very small interpreter and very low memory requirements. Because of this, it had a large influence late 1970's to early 1980's but it's popularity has waned.
  - It remains a strong influence on some other PL's
  - In 1994 a Forth standard was issued by ANSI known as **ANS Forth**, which is what we will use for the remainder of this class.
  - Forth words are **case-INsensitive**, as opposed to c++, lua, and Haskell.
  - Forth Syntax does not dinstinguish between variables, functions, and flow-of-control constructs. They're all just words.
  - Forth is **extensible** meaning new functionality has equal status with what already exists.
    - Forth does **not** have an extensible type system.
  - Extensibility is enabled via an internal Forth data structure: the **dictionary**. This lists all defined words, in the order they are defined.
  - Forth allows for **local** words to be defined within the definition of a word
  - Forth words (global) are **dynamically scoped**, they may be used at any time after their definition.
  - Forth supports types
    - Numbers (Floating) and Operations, Boolean, Pointers, Characters
  - A **parsing word** is a word that tells Forth to look at the next word. Used for things like "import myprog.fs"
  - Write helloworld.fs in Forth
    ```fs
    \ hello.fs
    \ Spencer Baysinger
    \ 2024-03-18
    \ Hello world program in Forth
    
    \ " s" This is a whole string " will create a string s
    \ "type s" will pull the string from stack and print
    \ s" Hello, World!
    \ type s

    \ You can print immediately w/out the use of a string with 
    \ ." Hello, World!

    cr. \ This will do something, idk
    ." Hello, World!
    ```
  - Alrighty, now lets do Fibonacci
    ```fs
    \ fibo.fs
    \ Spencer Baysinger
    \ 2024-03-18
    \ Fibonacci sequence mostly copied from Glenn Chappell in class demo

    : fibo { n --- F(n) }
      0 { currfib }
      1 { nextfib }

      n 0 ?do
        nextfib currfib nextfib + to nextfib to currfib
      loop

      currfib
    ;

    : printfibos { k-- }
      k 0 ?do
        ." F("
        i 1 .r
        ." ) = "
        i fibo .
        cr
      loop
    ;

    20 constant how_many_to_print
    cr 
    ." Fibonacci Numbers"
    cr
    how_many_to_print printfibos
    cr
    ```

# Week-8
[Top](#TOP)
## 2024-03-08
  - Midterm Test

## 2024-03-06
  ### Review
  - An **I/O action** is a value that holds a description of a sequence of zero or more side effects, plug a wrapped (potential) value
  - Do-expressions are syntactic sugar around operators for I/O actions
  ### Haskell: Data (cont'd)
  - Haskell calls IO a **type constructor**
    - ie. IO String -- A type: an IO action that wraps a string
  - Another standard type constructor is *maybe*, this allows us to make a value of an existing type, that either contains a value, or has a null value
    - ie. data Maybe t = Just t | Nothing
  - And another standard type constructor is *Either*, which allows us to make a value that holds one of two specified types.
    - ie. data Either a b = Left a | Right b

  - Let's write a Binary Tree
    - Such a binary tree either has no nodes (empty) or has a root node, with two subtrees.
    - Type BT will have two constructors
    - BTEmpty and BTNode
    - Recall that a binary search tree is a binary tree in which each node contains a single key;
      1. and every key in a nodes right subtree is greater than or equal to the node
      2. and every key in a nodes left subtree is less than or equal to the node
      ```haskell
      -- vt - Value Type
      data BT vt = BTEmpty | BTNode vt (BT vt) (BT vt)
      ```
      
  - Let's right a Treesort algorithm
    - **Treesort** is a comparison sort. It operates as follows, given a list:
      - Create an empty Binary Search Tree
      - Insert each list item into tree
      - Do an inorder traversal of the tree to generate the final sorted list.
      - Recall an inorder traversal will bisit the nodes left subtree, then the node itself, then the right subtree. Touching the base of a nodes "box" as it runs counter-clockwise.
      - Plan
        1. Function *bstInsert* takes a BT holding a Binary Search Tree and an item to insert, returns a BST with item inserted.
        2. Function *inorderTraverse*, takes a BT and returns a list of items in tree in order, making a recursive call on two subtrees and concatenates the results along with root item
        3. Function *treesort*, takes a list and returns a sorted list. Creates an empty BT and calls bstInsert with each item in the list, then calls *inorderTraverse* and returns it's results
    ```haskell
    -- vt = Value Type
    data BT vt = BTEmpty | BTNode vt (BT vt) (BT vt)

    bstInsert :: Ord vt => BT vt -> vt > BT vt
    bstInsert BTEmpty x = BTNode x BTEmpty BTEmpty
    bstInsert (BTNode root lsub rsub) x
      | x < root  = BTNode root (bstInsert lsub x) rsub
      | otherwise = BTNode root lsub (bstInsert rsub x)
    
    inorderTraverse :: BT vt -> [vt]
    inorderTraverse BTEmpty = []
    inorderTraverse BTNode root lsub rsub = 
      (inorderTraverse lsub) + root ++ (inorderTraverse rsub)
      -- () is returning a list, so we concatenate the 3 items returned above.

    treesort :: Ord vt [vt] => [vt] -> [vt]
    -- foldl: returns ((((x1 + x2) + x3) + x4) + x5) something like this
    treesort xs = inorderTraverse $ foldl bstInsert BTEmpty xs
    ```
  ### Forth PL Feature: Identifiers & Values
  - An **identifier** is the name of something in source code.
  - Every identifier lies in some **namespace
  - The code from which an identifier is accessible forms the idenifier's **scope**
  - **Static scope**: scope is determined before runtime.
  - This is typically **lexical scope**: The scope consists of a fixed portion of the program's source code.
  - **Dynamic scope**: scope is determined at runtime.
  - A **value** might be a number or a string or a Boolean or some kind of object
  - An **expression** is an entity that has a value.
  -  

## 2024-03-04
  ### Review
  - A Haskel **I/O action** holds a description of zero or more side effects plus a wrapped value.
    - Function *putStr* takes a *String* and returns an I/O action representing printing the String to the standard output.
  - Multiple I/O actions can be combined into one, which holds:
    - A description of all side effects from the combined I/O actions.
    - The wrapped value from the last of the combined I/O actions.
  ### Haskell: I/O (cont'd)
  - Haskell's **do-expression** offers a cleaner way to write I/O
  - a **Handle**: Object that identifies and allows access to an open file.
  - **Flush**: write any buffered characters
  - An **I/O action** holds a description of a sequence of side effects plus a wrapped value
  - A **do-expression** is nier. It is syntactic sugar arround >> and >>=.
  - To overload the "==" operator for type *Product*, we place this type into the *Eq* typeclass. We want type *Product* to be an **instance** of class *Eq*
    - In the **instance declaration**, we provide a definition of the "==" operator for *Product*.
      ```
      instance Eq Product where
        Pr pn1 mn1 == Pr pn2 mn2 = 
          (pn1 == pn2) && (mn1 == mn2)
      ```
    - Now we can use "==" operator with *Product*
  - If a Haskell program uses values obtained via input, what ends up being returned in the I/O action is a function call to run the *entire remainder* of the program.
    - This is to ensure that *purity* is not compromised.
    - This would effectively make any larger program of Haskell extremely slow, however, we can cut all of this nonsense out in the optimizer during compile time.

# Week-7
[Top](#TOP)
## 2024-03-01
  - In Vegas for Aivon's wedding.
  ### Review
  - 
  ### Haskell: I/O

## 2024-02-28
  - In Vegas for Aivon's wedding.
  ### Review
  - Haskell has two types of identifiers.
    1. **Normal identifiers** begin with lowercase, or underscore. These name variables, and functions.
    2. **Special identifiers** begin with an UPPER-CASE. These name modules, types, and constructors.
  - ToDo: MyMap and MyFilter
  - A **predicate** is a function that returns a boolean value.
  ### Haskell: Flow of Control
  - **Flow of control** refers to the ways a PL determines what code is executed.
    - Haskell has a **pattern matching** facility, which allows us to choose one of a number of function definitions. The rule is that the first defintion with a matching pattern is used.
  - Recursion in Haskell can be less costly, because it uses **tail-call optimization** (TCO), this means that tail call does not use additional stack space.
  - Haskell does **lazy evaluation**
  - **Selection** refers to flow-of-control constructs that allow us to choose one of multiple options to execute.
    - In C++ this could be if ... else, or switch
    - In Haskeel it's pattern matching. Other options could be if .. else .. then, *guard*, and a *case* construction.
  - **Guards** are the Haskell notation for the mathematical equivalent of a curly braced interval notation with multiple cases
    ```haskell
    myAbs x
      | x >= 0    = x
      | otherwise = -x
    ```
  - A very important idea in functional programming is that **Many flow-of-control constructs can be encapsulated as functions**
  - Another kid of a loop involves processing a sequence of values and returning a single value. The operation performed by a loop like this is called a **fold**, or a **reduce**

## 2024-02-26
  ### Review
  - Haskel has **Significant Indentation**. This signifies start and end of code blocks
  ### Haskell: Functions
  - Comments can be made with " -- ", and multi-line comments with " {- ... -} "
  - **Normal Identifiers** begin with a lower-case letter. These name variables and functions
  - **Special Identifiers** begin with an upper-case letter. These name modules, types, and constructors.
  - A function is **applied** to it's arguments; this is called **function application**
  - You can make special operators using any of the 20 operator values.
  - **Currying** is simulating a multiple-argument function using a single-argument function that returns a function.
    ```
    addem a b = a + b
    add2 = addem 2
    add2 3 -- returns 5    
    ```
  - A **Lambda function** is a kind of expression whose value is a function. A funtion with no name
    ```
    square x = x*x
    square' = \ x -> x*x -- Lambda function
    ```
  ### Haskell: Lists
  - Haskel **Lists** can hold an arbitrarily large number of data items of the same type
    - [] -- Empty list, [2,3,5] -- List of 3 ints
    - ["hello"] -- 1 String list -- [1, [2,3]] -- Error diff types
  - Because of lazy evaluation we can make infinite lists
    - [1, 3 ..] -- Infinite list of odd ints
  - Haskell **Tuples** hold a fixed number of data items, possibly of different types.
  - A **predicate** function returns a Boolean
  - **filter** takes a predicate and a list, and returns a list of items that pass the test
  - A **primitive** operation is a fundamental operation that other operations are constructed from. Haskell has three
    1. [] -- Empty list
    2. Cons: -- Construct a list given it's first item and a list of other items
    3. Pattern matching for lists
  - Haskell has **set comprehension** using a set-builder notation
    - { xy | x ∈ {3, 2, 1} and y ∈ {10, 11, 12}}
    - { x*y | x <- {3, 2, 1} and y <- {10, 11, 12}}
      - The above will output [30, 20, 10, 33, 22, 11, 36, 24, 12]
    ```haskell
    -- list.hs
    -- Spencer Baysinger
    -- 2024/02/26
    -- List stuff in Haskell
    -- Run interpreter and type to load program 
      -- :l list
      -- :r 

    -- Write a function isEmpty that determines whether a list is empty
    isEmpty []     = True 
    isEmpty [x:xs] = False

    -- Write a function listLength that returns the length of the list 
    listLength []       = 0
    listLegnth [x:xs]   = 1 + listLength xs 

    -- Write a function mymap that replicated Prelude function map
    myMap f []     = []
    myMap f (x:xs) = (f x):(myMap f xs)

    -- Write a function myFilter that replicates Prelude function filter
    isEven p

    myFilter p []     = 
    myFilter p (x:xs) = if (p x)
                          then (x:rest)
                          else rest where
                        rest = myFilter p xs

    ```

# Week-6
[Top](#TOP)
## 2024-02-23
  ### Review
  - Practical lexers and parsers run in linear time
  - A **Generalized LR (GLR)** parser runs something like a Shift-Reduce automaton. It allows multiple actions for a single state-input combination, and tries them all.
    - GLR's can handle all CFLs. It is cubic-time for many grammars, however, it does run fast for some, and slow for others.
  - **Parsing is making sense of input**
  - Things we are concerned about regarding PL's: Type, Static typing, Manifest typing vs. implicit typing, Type annotation, Type inference, First-class functions, Sound type system.
  - A PL or PL construct is **type-safe** if it forbids operations that are incorrect for the types on which they operate.
    - Avoid using **strong and weak** terms when speaking about typing.
  ### PL Category: Functional PL
  - **Imperative programming** is writing code to tell a computer what to do. This is typical. Examples; C++, java, lua
  - **Declarative programming** is more along the lines of telling a computer what is true, and can be thought of in terms of asking a question.
  - There is also **Logic programming** which we will cover later.
  - A function has a **side effect**, consider a const function returning a value, and a non-const function that may have a side effect. The term is used loosely to mean that it changes something.
  - A typical functional programming language will;
    1. Have first-class functions
    2. Offers good support for *higher-order functions*
    3. Has a preference for _immutable_ data
  - A **pure** functional PL goes farther, and does not support mutable data at all. There are no side effects.
  ### Introduction to Haskell
  #### History
  - The initial release of Haskell, named after **Haskell B. Curry (1900-82)**, was released in 1990.
  - It's purpose was to standardize the scientific functional programming community.
  - In 1998 Haskell 98 was released with two primary implementations;
    1. The **Haskell User's Gofer System (Hugs)**, a lightweight interactive environment.
    2. And the **Glorious Glasgow Haskell Compilation System**, aka the **Glasgow Haskell Compiler (GHC)**, a full-featured compiler. 
    - Both were eventually folded into **GHC** and was renamed to **Glasgow Haskell Compiler Interactive (GHCi)**
  - A second standard was released in 2010, called **Haskell 2010** aka **Haskell Prime**
  #### Characteristics
  - Haskell is a *pure* functional PL. It has first-class functions and excellent support for higher-order functions.
  - It has simple syntax with more or less equivalent function calls than c++, lua, and haskell.
    ```
    foo(a, b, c); // C++
    foo(a, b, c)  -- Lua
    foo a b c     -- Haskell
    ```
  - Haskell has no loops. Instead of iteration, it solely uses recursion.
  - By default Haskell does **lazy evaluation** only evaluating expressions and functions when literally used.
  
## 2024-02-21
  ### Parsing Wrap-up
  - When we analyze algorithms, we need to be clear on three things
    1. How we measure the size of the input (n)
    2. What operations are allowed.
    3. What operations we count (the **basic operations**)
  - Practical lexers and parsers run in **linear time**.
  - In the late 1960's and 70's, parsing methods were found that can handle all CFLs
    - Earley Parser [J. Earley 1968], CYK Parser [J. Cocke & J.T. Schwartz 1970, T. Kasami 1965, D.H. Younger 1967], and Generalized LR (gLR) Parser [B. Lang 1974, M. Tomita 1984]
    - All of the above model have a worst-case time of n^3 for an arbitrary CFL.
  - The syntax of Lua is specified with a CFG. The standard Lua implementation uses a hand-coded Recursive-Descent parser.
    - This parser does not costruct an AST. It emits bytecode directly, thus it's basically a compiler. This is called a **Shotgun Parser**
  - **Parsing Expression Grammar (PEG)** is an alternative to CFG. Using PEG you will use the _first_ production that works for grammar. So there is never more than one parse tree. PEGs have no issues with ambiguity.
  - The syntax of the Python PL is currently specified with a PEG. The primary implementation, **CPython**, uses a Recursive-Descent parser that is generated automatically from this PEG.
  - Parsing is making sense of input. This is an extremely common task that is in fact pretty important.
   ### Thoughts on Assignment 4
  - Turn in your Lexer with your Parser
  - This will create a Lexer and Parser for a language Dr. Chappell invented called **Nilgai**.
  - There will be 23 grammar states.
  - Remember to declare all variables at the beginning of your function using **local**.
  - **parseit.lua** parseit module will return two values: Boolean & AST.
    1. If bool is true, return AST in proper form
    2. If bool is false, return nil.
  - If you have a funciton that does something, and you need to do that thing, use the function. Don't write the same function twice.
  - Three files to help you: _parseit.lua_, _rdparser3.lua_, *use_parseit.lua*.
  ### PL Feature: Type System
  - An **Extensible** programming language is one that allows the programmer to define new types.
    - In c++ this is a class
  - **Type Checking** means checking & enforcing the restrictions associated with a type system.
  - The various actions involved with a type system (determining types, type checking) are collectively known as **typing**
  - Types are used in three ways  
    1. Which values an entity may take on
    2. Which operations are legal
    3. Which of multiple possible operations to perform.
  - We classify type systems along three axes.  
    1. Overall type system: **static** or **dynamic**
    2. How types are specified: **manifest** or **implicit**
    3. How types are checked: **nominal** or **structural**
    - We could also consider **Type Safety**
  - The following table shows various PL type systems
    |  | Type Specification |  |  |
    | -- | -- | -- | -- |
    |  |  | Mostly Manifest | Mostly Implicit |
    | Overal Type System | Static | C, C++, Java | Haskell, OCaml |
    |  | Dynamic | Not much goes on here | Python, Lua, Ruby, JavaScript, Scheme |
  - 
  

## 2024-02-19
  ### Review
  - Top-Down Parsers
    - Go through derivation top to bottom, expanding nonterminals.
    - Sometimes hand-coded and sometimes automatically generated.
    - Method we look at: Predictive Recursive Descent.
  - Bottom-Up Parsers
    - Go through the derivation bottom to top, reducing substrings to nonterminals.
    - Almost always automatically generated.
    - Method we look at: Shift-Reduce.
  - **Recursive Descent** is a top-down parsing method
    - **Predictive** = no backtracking. Predictive Recursive-Descent parsers that base decisions on _k_ lexemes use LL(k) grammars.
    - There is one parsing function for **each** non-terminal.
  - An **Abstract Syntax Tree** is usually the result output of a parser. This represents a series of operations.
    - There is no universal specification for an AST.
  ### Shift-Reduce Parsing
  - A **Shift-Reduce** parsing method is a bottom-up parsing method
    - It is usually automatically generated.
    - It was introduced by [D. Knuth 1965], and was not practical then. But now it is widely used.
    - The CFG's that a Shift-Reduce parser can handle are called **LR(k) grammars**. Lookahead is uncommon here.
    - LR(1) grammars are the larger category.
  - A **Shift-Reduce automaton** is a state machine with an associated stack. 
    - It is always based on a grammar, with respective states, each numbered.
    - Each stack item holds both a symbol from the grammar - either terminal or nonterminal - and a slate(number). The _current state_ is the state in the top stack item.
    - A Shift-Reduce automaton runs in a series of steps, each step performs one of the following actions;
      1. **Shift** - Shift the next input symbol onto the stack (push)
      2. **Reduce** Apply a production in reverse, reducing symbols on the stack to a single nonterminal, which replaces them on the stack
      3. **Accept** Done, successful 
      4. **Error** Done, unsuccessful
    - Operations of the automaton uses a **parsing table** in two parts
      1. The **action table** has a row for each state, column for each terminal
      2. The **goto table** has a row for each state and a column for each nonterminal.


# Week-5
[Top](#TOP)
## 2024-02-16
  ### Review
  - **Recursive Descent** is a top-down parsing method
  - **Predictive** = No backtracking.
    - There is one parsing function for each nonterminal.
  ### Recursive-Descent Parsing (cont'd)
  - Let's consider an example of proper (Ba) and improper (B) LL(1) grammar
    - **Not LL(1)** xx -> xx "+" "b" | "a"
    - **LL(1)** xx -> "a" yy
                yy -> "+" "b" yy | ""

## 2024-02-14
  ### Thoughts on Assignment 3
  - The Lexer, Parser and Interpreter will be used for a language called **Nilgai** which was invented by Dr. Chappell for CS331
  - **Legal** characters are whitespace & printable ASCII
  - 7 lexeme categories
    1. Keywords
      - and, char, def, else, elseif, eol, false, if, inputnum, not, or, output, rand, return, true, while
      - and, or, not are operators
      - false and true are boolean literals
    2. Identifiers
      - Identifiers cannot be Keyword lexeme.
    3. NumericLiteral
      - is a sequence of digits, optional to be followed with an "e" / "E" + "+" digit.
      - No dots, and exponents cannot contain "-" a hyphen
      ex. "111" "7e31" "37E+1"
    4. StringLiteral
      - Any sequence of ASCII literals and whitespace
    5. Operator
      - 14 Operators: == != < <= > >= + - * / % [ ] =
    6. Punctuation
      - Any single character that is not whitespace, not part of comment, and not part of a lexeme in any of the other categories.
    7. Malformed
      - Bad character: A single illegal character that is not part of a comment or StringLiteral
      - Bad string: Something that would be a StringLiteral but there is a newlne or the input ends before the ending quote appears.
        - To check for bad string, you will need to check for the end of the input. This is done by checking the current character is the empty string.
  ### Review
  - All grammar based parsers go through the steps required to parse.
  - **Top-Down** Parsers
    - Derivation top to bottom, **expanding** nonterminals.
    - Category are: **LL parsers** read input left to right, steps produce Leftmost derivation.
    - Method we look at: **Predictive Recursive Descent**
  - **Bottom-Up** Parsers
    - Derviation from bottom to top, **reducing** substrings to nonterminals
    - Category are: **LR parsers** Left to right, steps produce Rightmost derivation.
    - Method we check: **Shift-Reduce**
  - All LL(1) grammar is an LR(1) grammar. Not all LR(1) grammar is LL(1) grammar
  ### Recursive-Descent Parsing (cont'd)
  - Remember that [brackets represent optional (0 or 1) things] These are conditionals (ifs)
  - { Braces} represent repeatable, 0 or more items become a loop (while)
  

## 2024-02-12
  ### Review
  - When we write a state machine, when do we add a new state? **Two situations can be handlded by the same state if they should react identically to all future input**
  ### State-Machine Lexing (cont'd)
  - When there are lexeme situations that are tricky to handle, the best way to deal with it is to **Lookahead**, not **Backtracking**
  - When dealing with errors in functions, there are three places we can deal with them. **Before, During, or After**
    - We don't want to handle errors **before the function** because this would require a preprocessing step before the lexer, that's inconvenient.
    - We don't want to handle errors **during the function**, this can only happen if our lexer 'fixes' illegal characters by changing, or skipping them. This would change the definition of a syntactically correct program
    - **After is good**?
  ### The Basics of Syntax Analysis
  - A parser will read in the lexeme stream from the lexer. A Parser will do the following
    - Determine if the input is syntactically correct.
    - If it is not correct, then output info about issue
    - If correct, output some representation of it's structure, typically as an **abstract syntax tree**
  - Parsing methods can vary a great deal, but they usually come in two forms: **top-down** and **bottom-up**.
    - Top-down: Goes through the derivation from top to bottom, beginning with start symbol, **expanding** nonterminals as it goes, and ending with the string to be derived.
      - Usually expand the leftmost nonterminal first, usually producing leftmost derivations.
    - Bottom-up: Goes through the derivation from bottom to top, beginning with string to be derived **reducing** substrings to nonterminals as it goes, and ending with the start symbol.
  - **LL Parsers** handle input in a left-to-right order, thus top-down parsers, they go through the steps to generte a Leftmost derivation. They are LL parsers. **Predictive Recursive Descent** lies in the LL category as well.
  - **LR Parsers** handle input in a left-to-right order, but make irrevocable decisions at each step and generate Rightmost derivations. These are bottom-up parsers. There exists **Shift-Reduce** parsing methods similar to this. 
  ### Recursive-Descent Parsing
  - A Recursive-Descent parser consists of a number of **parsing functions**. There is one parsing function for each nonterminal.
  - Let's look at [rdparser1.lua]() & [use_rdparser1.lua]()

# Week-4
[Top](#TOP)
## 2024-02-09
  ### State-Machine Lexing (cont'd)
  - A guiding principle to writing a new state machine: **_Two situations can be handled by the same state if they should react identically to all future input._**
  
## 2024-02-07
  ### The Basics of Lexical Analysis
  - Consider a stream of characters, and the role a Lexer and a Parser play.
    - The Lexer is the first process that the character stream goes through, it's output is a Lexeme Stream.
    - Then the Parser takes in the Lexeme Stream and it's output is an **Abstract syntax tree** or Error.
  - These two phases are **Lexical Analysis (lexing)** and **Syntax Analysis (parsing)** 
    ![lexer-parser](https://github.com/sowens23/CS-F331/blob/main/images/lexer-parser.png)
  - **Lexemes** are usually classified by category
  - An **idenfifier** is a name that a program gives to some entity like "class **MyClass**"
  - A **keyword** is an identifier-looking lexeme that has special meaning "**class** MyClass"
  - An **operator** is a wn alternate method for making something like a function call "int a **+=** b"
  - The **arity** of an operator is the number of characters the operator uses
    - **unary** operator " **+** "
    - **binary** operator " **+= "
      - A binary operator placed between its operands is an **infix** operator
    - A **ternary** operator has three operands. They are uncommon. None in Lua, but in C++ "**... ? ... : ...**"
  - A **literal** is a representation of a fixed value in source code.
    - **C++ Literals**
      | Literal | Type |
      |---|---|
      | 42 | int |
      | 42.5 | double |
    - **Lua Literals**
      | Literal | Type |
      |---|---|
      | 42.5 | number |
      | false | boolean |
  - **Punctuation** is the category for the extra lexemes in a program that do not fit into any of the previously defined categories. In C++, these are " {} ", " ; ". or " : ".
  - Lexeme categories listed above
    - **Identifier**
    - **Keyword**
    - **Operator**
    - **Literal**
    - **Punctuation**
  - A **Reserved Word** is a word that has the general form of an identifier, but is not allowed as an identifier.
  - A lexer outputs a sequece of lexemes
  ### State-Machine Lexing
  - The **Maximal Munch** rule says that a lexeme is always the longest substring beginning from it's starting point that can be interpreted as a lexeme.
  - Internally (Inclass), the written lexer will operate as a **state machine**.
    - A state machine has a current **state**. This might simply be a number. Code that runs as a state machine will need to store this.
    - The machine proceeds in a series of steps. At each step, it looks at the current item (here, character) in the input and the current state. It then decides what state to go to next.
    - Based on the state and current character, our state machine may also make other decisions.
  - There are variables (operations)  
    1. Input is the given string: *program*
    2. The index of the next character to read is stored in variable *pos* which starts at 1 in Lua.
    3. The state is stored in variable *state*, initialized as *START**
    4. We build a lexeme in string *lexstr*, initialized as empty ("")
    5. The category of a complete lexeme is stored in *category*
  - When a complete lexeme has been found, set *state* to *DONE*, and set *category* appropriately.
  - Let's write a lexer [lexer.lua](https://github.com/sowens23/CS-F331/blob/main/inclass/lexer.lua)
  - Utility Functions (Exception parameters)
    - To add the current character to the lexeme, call *add1()*
    - To skip the current character without adding it, call *drop1()*
    - Lua has no character type. We represent a character as a string of length one. We can test to see if something is a valid character by using functions like (*isLetter()*, *isDigit()*, *isWhitespace()*, *isIllegal()*). Each takes a string. When given a string whose length is not exactly one, each returns false.
  - It might be easiest to classify all lexemes as keywords, then go through once all lexemes have been processed and pick out the identifiers.
  
## 2024-02-05
  ### Review
  - A closure is a function that carries with it a reference t or copy of the environment in which it was created.
  ### Lua: Advanced Flow
  - A **coroutine** is a function that can temporarily give up control (**yield**) at any point and then later be **resumed**.
    - Each time a coroutine temporarily gives up control, it may pass one or more values back to it's caller.
    - An example of this, is a loop from the caller running calculations, then running a coroutine to use the callers values. The coroutine also runs a loop, and yields itself to send back a value, then both the caller and the coroutine will send data back and forth, resuming each state where it was left off, continuing calculations on a persistent value.
  - In Lua, a *coroutine* contains two functions
    - coroutine.yield
      - A coroutine yields, sending values back to caller, which then can be *resumed*. It only, actually returns when it's finished, which then can no longer be *resumed*
      - We know coroutine is finished when it ... something *nil*
    - coroutine.wrap
      - The caller in Lua does not call a coroutine directly. Instead it uses coroutine.wrap to pass a coroutine function to coroutine.wrap. The return value is a **coroutine wrapper function**; call this to call/resume the coroutine. The *first* time the wrapper function is called it's arguments are passed to the coroutine function.
  - [Lua Fibo CoRoutine](https://github.com/sowens23/CS-F331/blob/main/inclass/)
      ```lua
      -- working.lua

      -- We want to calculate a user defined number of fibo numbers from 0, to x.
      -- Well do this in steps
      -- 1) Write a function that prints that sequence of values
      -- 2) Then update the code, to operate with a coroutine

      -- limit is the input value
      function small_fibosl(limit)
        local currfib, nextfib = 0, 1
        while currfib <= limit do
          -- Here we will return currfib
          coroutine.yield(currfib)
          currfib, nextfib = nextfib, currfib + nextfib
        end
      end

      max_fibo = 3000

      cw = coroutine.wrap(small_fibo)

      io.write("Small fibos (coroutine):\n")
      f = cw(max_fibo)

      while f ~= nil do
        io.write(f.."  ")
        f = cw()
      end

      io.write("\n")

      ```
    - A Lua **iterator** is a function that is called repeatedly. We can do this by defining a function.
      ```lua
      function MYFUNCTION(...)
        local ... -- iter_func is a closer, so we can create variables here to store info between calls to iter_func, if we need to
        local function iter_func()
          if ... then
            return nil -- Iterator exhausted
          end
          ...
          return ... -- Return next value (s)
        end
        return iter_func
      end

      -- Then we can call the iterator function here
      for k in MYFUNCTION(...) do
        -- Iterator stuff
        -- This loop will continue until the function returns nil 
      end
      ```
    - Lua Iterator Fibo
      ```lua
      function small_fibos2(limit)
        local currfib, nextfib = 0, 1
        function iter_func()
          if currfib > limit then
            return nil
          end
          local save_curr = currfib
          currfib, nextfib = nextfib, currfib + nextfib
          return save_curr
        end

        return iter_func
      end

      -- now we have to make a loop that runs through the iterator
      max_fibo = 3000
      io.write("Smal fibos (iterator):\n")
      for f in small_fibo2(max_fibo) do
        io.write(f.."  ")
      end 
      io.write("\n")
      
      ```
    - Glenn Chappel, 2024 "If someone pointed a gun at me and said, write embedded software for a pacemaker or I'll blow your head off, I'd write it in C"
  ### Unit Overview - Lexing & Parsing
    - Our third unit: **Lexing & Parsing** topics: Introduction to lexing & parsing, the basics of lexical analysis, State-machine lexing, the basics of syntax analysis, recursie-descent parsing, shift-reduce parsing, parsing wrap-up
  ### Intro to Lexing & Parsing
    - Here are some things a compiler needs to do:
      1. **Determine whether the iven program is syntactically correct, and, if so, find it's structure.**
      2. Determine all identifiers and what they refer to.
      3. If compiling code in a statically typed PL, determine types and check that no typing rules are broken.
      4. Generate code in the target language.
    - **Parsing**: Determining whether input is syntactically correct, and, if so, finding it's structure.
    - Software that does parsing is called a **parser**.
    - A parser outputs most commonly an **abstract syntax tree (AST)**. This tree usually leaves out things like punctuation, which only serve to guide the parser, or human readers.
    - A preprocessing step is often split off from parsing: **lexical analysis** or **lexing** where input is split into **lexemes** (words, roughly), and the category of each is determined. Things like whitespace and comments are usually skipped.

# Week-3
[Top](#TOP)
## 2024-02-02
  [checklua.lua](https://github.com/sowens23/CS-F331/blob/main/inclass/)
  ### Review
  - In Lua, only *false* and *nil* are **truthy**. All other values are **falsy**.
  ### Lua: Objects
  - You can make tables. Tables can hold basically anything, including functions.
  - We can also make a meta-table, which is basically another table, associated to the linked primary table.
    ```lua
    -- pets.lua
    -- Spencer Baysinger
    -- 2024-02-02
    -- Source for "class" Dog (and others?)

    -- Here is the primary table
    local pets = {}

    -- Here is a meta table??
    pets.Dog = {}
    -- Dog index
    function pets.Dog.__index(tbl, key)
      return pets.Dog[key]
    end

    function pets.Dog.new(barkSound)
      if barkSound == nil then 
        barksound = "Ni!" -- We are the dogs that say "Ni!"
      end

      local obj = {}
      setmetatable(obj, pets.Dog)
      obj._sound = barkSound
      return obj
    end

    function pets.Dog.bark(self)
      io.write(self._sound.."\n")
    end 

    function pets.Dog.setBark(self, new_sound)
      self._sound = new_sound
    end

    return pets
    ```
    ```lua
    --- working.lua
    pets = require "pets"

    Dog = pets.Dog

    rover = Dog.new()
    fifi = Dog.new("Yip! Yip! Yip!")
    bruiser = Dog.new()
    bruiser:setBark("RRRRRROWF!!")

    io.write("Rover barks: ")
    rover:bark()
    ```
  - In regard to operator overload. You can't normally do arithmetic with TABLE OBJECTS. But you can overload the '+' and '-' operators, as a table function, to do arithmetic on other table functions.
  
## 2024-01-31
  ### Missed class h/

## 2024-01-29
  [checklua.lua](https://github.com/sowens23/CS-F331/blob/main/inclass/check_lua.lua)
  ### Review
  - EBNF typically includes two important features
    - Brackets "[]" surround optional sections
    - Braces "{}" surround optional, repeatable sections
  ### PL Category: Dynamic PLs
  - Batch File Syntax
    ```
    curl -o http://b.us/dog.c
    curl -o http://b.us/cat.c
    curl -o http://b.us/asp.c
    curl -o http://b.us/bat.c
    ```
  - Script Shell (Bash) Syntax
    ```
    for n in dog cat asp bat eel
    do 
      curl -o http://b.us/$n.c
    done
    ```
  - To improve scripting languages, **AWK** named after it's authors A. Aho, P. Weinberger, and B. Kernighan was developed in Bell Labs in the 1970's was one of the first scripting languages*?
  - In 1987 **Perl** was released, based on AWK, developed by Larry Wall. One of the first full-featured PLs w/ data structures and could access operating system services.
  - Then came Python 1991, Lua 1993, Ruby, JavaScript and PHP in 1995.
  - All the above languages can be considered **dynamic PLs**
  ### Introduction to Lua
  - Lua was developed in 1993 in Brazil to combat inaccessbile PLs from other countries. It was based on SOL (Simple Object Language) and had a meaning of sun, so they named in Lua, which means moon.
  - Lua consists of much less punctuation that other languages.
    ```
    function fibo(n)
      local currfib, nextfib = 0, 1
      for i = 1, n do
        currfib, nextfib = nextfib, currfib + nextfib
      end
      return currfib
    end
    ```
  - Lua has exactly 8 types.
    - Number, String, Boolean, Table, Function, Nil, Userdata, and Thread. You cannot define more types than this.
  - **Eager evaluation** is when an expression is evaluated when it is encountered during execution. This is how Lua works.
  - **Lazy evaluation** is when an expression is evaluated only when it's value is needed.
  - Let's program in Lua
    ```
    #!/usr/bin/env lua
    -- hello.lua
    -- Spencer Baysinger
    -- 2024/01/29
    -- Hello World in Lua

    io.write("Hello, world!\n")
    ```
  - Fibo in Lua
    ```
    function fibo(n)
      local currfib, nextfib = 0, 1
      for i = 1, n do 
        currfib, nextfib = nextfib, currfib + nextfib
      end
      return currfib
    end

    how_many_to_print = 20

    io.write("Fibbonacci Numbers\n")
    for i = 0, how_many_to_print-1 do
      io.write("F("..i..") = "..fibo(i).."\n")
    end

    io.write("\n")
    ```

# Week-2
[Top](#TOP)
## 2024-01-27 Weekly Quiz, and Homework 1 Notes
  - If there is more than one path that a nonterminating string can take in Context Free Grammar, it's considered **ambiguous**
  - Lua Syntax Notes?
    - Lua has 8 hardcoded types, but users can make infinitely more.
    - Lua multiple assignment
      ```
      // u = x, and v = y
      u, v = x, y
      ```
    - Lua not equal operator: =~
    - Lua Valid string literals: 'emu' , [[frog]] , [==[zebra]==]
    - Lua Valid and operator: and
    - Lua keyword for else if: elseif
    - Lua keywork to introduce new function: function
  - An example in Regular Language Expression: Consider regular expression (a|x)*cb
    - (a|x) matches either a, or x
    - * matches the concatenation of zero of more strings of the object before
    - cb concatenates 'cb' at the end of this string
    - ex. 'cb' 'aaaaxcb' axxxxcb' xxxcb'
  - **Ambiguity** is a property of grammars (CFGs).
  - **Inherent ambiguity** is a property of languages (CFLs)

## 2024-01-26
  - **Context-free grammar (CFG)** is a grammar, each of whose productions has a left-hand side consisting of a single non-terminal
  - **Context-free language (CFL)** is a language generated by context free grammar
    - CFG's are usually powerful enough to specify the context of most programming languages
    - They are ths important in **parsing**: determining whether given input is syntactically correct, and if so, finding it's structure
  - A CFG is **ambiguous** if some string has multiple parse trees

  ### Backus-Naur Form
  - We want a grammar format that:
    - Can deal with terminals involving arbitrary character sets
    - Does not require unusual characters
    - Is suitable for use as input to a computer program
    - Allows symbols to have descriptive names
  - One solution to the above is: **Backus-Naur Form (BNF)**
    - A notation for writing context-free grammar
    - Used to specify syntax of many programming languages
    - ex. <phone-number> ::= <area-code> <digit7> | <digit7>
          <area-code> ::= "(" <digit> <digit> <digit> ")"
          <digit7> ::= <digit> <digit> <digit> "-" <digit> <digit> <digit> <digit> 
          <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
      - The above specifies language as such, among other
        - (907) 474-5736
        - 555-1234
  - Many variations of **BNF** have been proposed, some referred to as **extended BNF or EBNF**
  - The **Lexical Structure** of a programming language is about how a program in broken into **lexemes**: identifiers, operators, keywords, etc.

  ### Intro to Survey of Programming Languages
  - A **programming language** is a notation for specifying comptations.
  - A complete specification is called a **program**
  - **Entity** described an arbitrary *thing* in a program: a variable, expression, function, class, etc.
  - An **expression** is an entity that has a value
    - -45.4, x, (3+g/6)*k, ff(z), "Ostrich"
  
  ```
  c++
    #include <iostream>
    int main () {
      std::cout << "Hello" << std::endl;
    }

  Lua
    io.write("Hello, world!\n)

  Haskell
    module Main where
    main = putStrLn "Hello, world!"

  Forth
    ." Hello, world!" cr

   Scheme 
    (display "Hello, world!")
    (newline)

  Prolog
    main :- write('Hello, world!'), nl.
  ```
  - 

## 2024-01-26
  - Sick, zoomed in using discord and another student

## 2024-01-22
  ### Review (Formal Languages)
  - A **formal language** is *only* a set of strings.
    - C++ is **not** a formal language.
  - We can specify the syntax of **regular expressions** or (**regexes**) by showing how to build them from small pieces.
    - We can list the individual pieces ex. a single character: a, and an empty string $$\\epsilon$$

# Week-1
[Top](#TOP)
## 2024-01-21
  ### Extra Reading (Compilers & Interpreters)
  ### Runtime
  - **Runtime** occurs during program **execution**, the computations it specifies, actually occur.
  - **Runtime system** is code that assists in, or preforms, execution of a program.
    - Typically includes low level I/O operations, and **memory management**
    - C++ *new* and *delete* call code in the runtime system
    - In languages like Lua, Python, and Java there exists runtime systems that do **garbage collection** which cleans up no-longer-used memory blocks
  ### Compilers
  #### Introduction
  - A **compiler** turns **source language** (programming language) and turns it into the **target language**. This is called **compilation**.
  - A compiler might target **native code** (machine language directly executed by a computer's processor) or it may target another programming language.
  - **byte code** are programming languages that are intended solely as target languages which are not aimed at readability for humans. 
    - Codes like this include; **Java byte code** which is executed by **Java virtual machine** (JVM)
  - Generally we only use the term "compiler" when all of the following are true;
    1. The source and target languages differ significantly.
    2. The target language is lower-level than the source.
    3. The transformation is done with execution as the goal.
    - If this is not the case, then technically you might refer to it as a **preprocessor** or an **assembler**.
    - Software that compiles code which operates at the same level of abstraction may also be called a compiler, but more accurately would be described as a **transcompiler** or **transpiler**
  #### Multi-Stage Compilation
  - Good compilers will proceed in a number of distinct steps. Transforming code into **intermediate representations** (IR), which ultimately becomes the target language
    - For example, the C++ compiler used by Apple's Xcode is called **Clang**, which does not target native code, but intermediate machine-independent code specified by the **Low-Level Virtual Machine (LLVM)** project, this intermediate code is then converted to native code. 
  - There are advantages to breaking down your compilation process into multiple stages.
  - To 
  #### Optimization
  - To **optimize** code means to transform it so that it still performs the same task, but is better somehow, usually faster.
  - A compiler that can perform optimization is an **optimizing compiler**. Today, most major compilers are optimizing.
  - Many integrated development enviroments (IDEs) have debug build and a release build.
    - The major difference is that some optimizations are only in the release build.
  ### Interpreters
  - An **interpreter** takes code in some language, and executes it. This is called **interpretation**.
  - Some interpreters allow code to be executed in an **interactive environment** where a use can type in a statement to have executed, and then the interpreter will request another expression, or code block. This is called **REPL (Read-Eval-Print Loop)**, the term comes from Lisp family of programming languages
  ### JIT Compilers
  - Some code transformations require information only available at runtime. This can be referred to as **profile-based optimizations** which transform code based on portions of the code which spend most time executing. Newer compilers can perform these transformations while the code runs, this is called **dynamic compilation**.
  - Dynamic compilation is commonly referred to as **Just-In-Time (JIT)** compilation, and it's compilers called **JIT Compilers**, or simply **JIT**.
  - A typical strategy for a JIT is to do static compilations of source code into byte code. Then when execution begins, the byte code is replaced by native code, chunk by chunk, which preforms the tasks more quickly.
    - This is in fact how LuaJIT does JIT compilation.

## 2024-01-19
  ### Review
  - **Dynamic**: *at runtime*.
  - **Static**: *before runtime*.
  - **Syntax**: the correct *structure* of code.
  - **Semantics**: the meaning *meaning* of code.
  - A (**formal**) **language** is a set of *strings*.
    - Not the same as a programming language!
    - Examples of formal languages:
      - The set of all lexemes in some category, for some programming language (e.g. the set of all legal C++ identifiers)
      - The set of all syntactically correct programs, in some programming language (e.g., the set of all syntactically correct Lua programs)
  - **Alphabet**: the set of characters that may appear in the strings.
  - To describe a formal language with a **generator**
    - Are easier to use, and is something that can produce the strings in a formal language, all of them, nothing else
  - To describe a formal language with a **recognizer**
    - Recognizers are more useful, and are a way of determining whether a given string lies in the formal language.
  - It's common to start with a constructor, then construct a recognizer based on it.
  - A (**phrase-structure**) **grammar** is a kind of language generator, which needs;
    - A collection of **terminal symbols**, our alphabet
    - A collection of **nonterminal symbols**, which turn into something else. 
    - And usually one nonterminal symbol is the **start symbol**
    - A **grammar** is a list of one or more *productions*
    - A **production** is a rule for altering strings by substituting one substring for another, made of terminal and nonterminal strings.
  - The result is a **derivation** of the final string
  - The final string will usually consist of *only* terminal strings.
  - FOR CLASS: Terminal (a b x ), nonterminal (C Q S), start symbol (S)

  ### The Chomsky Hierarchy
  - The Chomsky Hierarchy consists of four categories of languages.
    | Language Category | | Generator | Recognizer
    | -- | -- | -- | -- |
    | Number | Name | | |
    | Type 3 | Regular | Grammer in which each production has one of the following forms. A -> ε, A -> b, A -> bC. Another kind of generator: *regular* expressions (covered later). | Deterministic Finite Automation, Think: Program that uses a small, fixed amount of memory. |
    | Type 2 | Context-Free | Grammar in which the left-hand side of each production consists of a single nonterminal. A -> [anything]. | Nondeterministic Push-Down Automation. Think: Finite Automaton + Stack (roughly) |
    | Type 1 | Context-Sensitive | Don't worry about it. | Don't worry about it. |
    | Type 0 | Computably Enumerable | Grammar (no restrictions). | Turing Machine. Thik: Computer Program | 
  - The Chomsky Heirarchy is contained in the next, So every regular language, is context-free, etc.
  - A **Regular Language** is one hat can be generated by a grammar is the above described forms.
    - Alternative generator: **regular expression** (covered later)
    - Can be recognized by a **deterministic finite automaton**
    - Describe lexeme categories
    - Ecompass the level of computation required for **lexical analysis**: breaking a program into lexemes
    - Used in text **search/replace**
  - A **context-free language** can be recognized by a **nondeterministic push-down automaton**
    - These languages encompass the level of computation required for **parsing**
    - Determine whether a program is syntactically correct, and how it is structured.
  - A **context-sensitive language**, we generally do not care.
    - Not a fruitful thought of Chomsky
    - The recognizer for this type of language is called a *linear bounded automaton*
  - A **computably enumerable language** is one described by grammar with no restrictions on the production.
    - The recognizer for this would be a **turing machine**.
    - These languages encompass the things that computer programs can do
    - These languages are **recursively enumerable language**, derived from a branch of mathematics called *recursively enumberable language*

  ### Regular Languages
  - The smallest of the Chomsky Heirarchy.
  - Regular languages have two important applications
    1. lexical analysis, or lexing.
    2. text search/replace
  - Regular Grammar production rules will be in this fashion; 
    1. A -> ε
    2. A -> b
    3. A -> bC
  - To see if regular grammar is correct, you can look into **Pumping Lemma for regular language**
  - A **deterministic finite automaton** (Latin plural "**automata**") or **DFA**
    - Is a recognizer for regular languages.
    - Consists of a finite collection of **states** and **transitions**
      1. One state is the start state
      2. Some states my be accepting states
      3. Each transition begins at a state, ends at a state, and is associated with a terminal symbol
      4. for each character, each state has exactly one transition leaving it
    - The set of all inputs that are accepted, is the language recognized by the DFA
    - Languages that are recognized by DFA's are precisely the regular languages
  - **Zero is an even number.** But a string with no characters (ε) is not a permissible identifier.

## 2024-01-17
  ### Course Objectives
  1. Understand the concepts of syntax and semantics
  2. Understand implementing lexical analysis, parsing and interpretation
  3. Understand various types of programmiing languages
  4. Understand programming language features and the forms they take
  5. Be familiar with the impact of choosing a programming language
  6. Develop a basic proficiency in multiple programming languages

  - We will learn how to use
    1. Lua (ZeroBrane Studio)
    2. Haskell (GHC)
    3. Forth (GNU version of Gforth)
    4. Scheme (DrRacket)
    5. Prolog (SWI-Prolog)

  - We will focus on;
    - Syntax: correct structure
    - Semantics: meaning
    - And features of programming languages
  - There will be;
    - 13 quizzes, 7 homework assignments, and 2 exams
  
  ### Basic Concepts of Course
  - Specification: is a precise description
  - Dynamic: Refers to things that happen at runtime
  - Static: Refers to things that happen before runtime
  - Syntax: is the correct structure of code
  - Semantics: is the meaning of code
    - Static Semantics: are issues that occur before runtime
    - Dynamic Semantics: are issues that occur during runtime
  - A string is a finite sequence of zero or more characters
  - Formal Language is a set of strings
    - We can denote an empty string "" as ε (Greek epsilon)
  - There are two broad categories of ways to describe formal languages;
    - **Generators**: are something that can produce the strings in a language
    - **Recognizer**: is a way of determining if a given string lies in the language
  - A **Phrase-Structure grammar** (or just grammar) is a kind of language generator. It's a list of one or more productions.
  - A **production** is a rule for altering strings by substituting one substring for another. These are made of terminal and nonterminal symbols
  - **Terminal symbols** are what we use to compose our grammar
  - **NonTerminal symbols** are like variables that eventually turn into something else. One nonterminal symbol is the **start symbol**

  ### Derivations
  - A **derivation** is the result of strings left by the final strings
  - If we start with "S"
    - Grammar rules are: S -> xSy, S -> ε
    - We can turn this into an infinite multiple of 
  
  - Grammar containing  strings = {ε}
  - Grammar containing