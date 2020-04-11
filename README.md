# Hello Erlang!
These are notes and quotes i take about Erlang while
reading the [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) book.

# Topics
<details>
  <summary><strong>numbers</strong></summary>

  both floating point numbers or integers are supported when dealing with arithmetic. Integers and floating values are pretty much the only types of data Erlang's mathematical operators will handle transparently for you. to have the integer-to-integer division, use `div`, and to have the modulo operator, use `rem`.

    5 / 2.
    >> 2.5

    5 div 2
    >> 2

  to express integers in other bases than base 10, just enter the number as Base#Value (given Base is in the range 2..36):

    2#101010.
    >> 42

    8#0677.
    >> 447

    16#AE.
    >> 174

</details>

# Definitions
<details>
  <summary><strong>referential transparency</strong></summary>
  An expression is called referentially transparent if it can be replaced with its corresponding value without changing the program's behavior. This requires that the expression be pure, that is to say the expression value must be the same for the same inputs and its evaluation must have no side effects. An expression that is not referentially transparent is called referentially opaque.
</details>

# Links
- [Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
- [Why did Alan Kay dislike Java](https://www.quora.com/Why-did-Alan-Kay-dislike-Java)

# Setup
#### install erlang
    brew install erlang
#### run shell to test it
    erl
#### using shell
  - expressions have to be terminated with a period followed by whitespace (line break, a space etc.), otherwise they won't be executed
  - `help().` print help
  - `q().` quit
  - `ctrl+g` abort menu
  - `ctrl+a` go to beginning of line
  - `ctrl+e` go to end of line
  - `tab` complete
