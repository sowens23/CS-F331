#import "@preview/finite:0.3.0": automaton



= Spencer Owens
=== CS F331 - Assignment 1

#set enum(numbering: "A.1.a)")
+ Secret Message: BE SURE TO DRINK YOUR OVALTINE

+ 
    + In C++, I think the type checking is primarily static.
    + This means that type checking for C++ is primarily done during compile time, or atleast before runtime. As opposed to dynamic, which is done during runtime

+ 1, 3, 5.

+ This language contains strings that;
    + An equal number of zero or more x's on the left hand side, and z's on the right hand side.
    + An even number of y's in the middle (this include 0 y's). \
        ex. '', 'xz', 'xyyz', 'xxxzzz', 'xxxxyyyyyyzzzz'

+ 3, 4, 5.

+ y+(x|y|z)\*

+
    + Axy
    + xyA
    + S --> SS --> 1|2 (see 1 and 2 parse tree banches below) \
      SS --> xyS \
      SS --> Sxy \ 
    + S --> A | B \
      A --> xy \
      B --> SB | S \

+
    + (a)(a+)(b)
    +   #automaton(
            (
                S: (X:"aa"),
                X: (X:"a", E:"b"),
                E: (),
            ),
            layout: (
                S: (0,0), 
                X: (rel:(2, 0)),
                E: (rel:(2, 0))
            ),
            labels: (
                S: [],
                X: [],
                E: [end]
            )
        )
    + S --> ABC \
      A --> aa \
      B --> aB | \u{03F5} \
      C --> b \
    + No, because there is only one parse tree.

+ \<reg-exp> ::= \<char> | \<char> \<reg-exp> \
  \<char-non-terminals> ::= \<char>