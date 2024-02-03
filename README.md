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