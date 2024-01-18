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
## 2024-01-17
  - 
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