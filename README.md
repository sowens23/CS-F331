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

### Big Notes
  - 

# Class Notes and Assignments
  | Weekly Notes | Assignments | Other Notes |
  | --- | --- | --- |
  | [Week-1](#Week-1) | | |

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