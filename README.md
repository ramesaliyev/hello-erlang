# Hello Erlang!
These are notes and quotes i take about Erlang while
reading the [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) book.

# Topics
<details>
  <summary><strong>numbers</strong></summary>

  both floating point numbers or integers are supported when dealing with arithmetic. Integers and floating values are pretty much the only types of data Erlang's mathematical operators will handle transparently for you. to have the integer-to-integer division, use `div`, and to have the modulo operator, use `rem`.

    5 / 2.
    -> 2.5

    5 div 2
    -> 2

  to express integers in other bases than base 10, just enter the number as Base#Value (given Base is in the range 2..36):

    2#101010.
    -> 42

    8#0677.
    -> 447

    16#AE.
    -> 174

</details>
<details>
  <summary><strong>invariable variables</strong></summary>

  variable names must begin with a capital letter. you can assign a value to a variable exactly once. you can `pretend` to assign a value to a variable if it's the same value it already has.

    One = 1.
    Two = One + One.
    Two = 2.
    Two = 3.
    -> ** exception error: no match of right hand side value 3

  the `=` operator has the role of comparing values and complaining if they're different. If they're the same, it returns the value.

    47 = 45 + 3.
    -> ** exception error: no match of right hand side value 48

  if the left-hand side term is a variable and it is `unbound` (has no value associated to it), Erlang will automatically bind the right-hand side value to the variable on the left-hand side. The comparison will consequently succeed and the variable will keep the value in memory.

  the `=` operator is the basis of the `pattern matching`

  technically, variables can start with an underscore too, but by convention their use is restricted to values you do not care about, yet you felt it was necessary to document what it contains.
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
  - `f(Variable)` clear a variable
  - `f()` clear all variables
  - `ctrl+g` abort menu
  - `ctrl+a` go to beginning of line
  - `ctrl+e` go to end of line
  - `tab` complete
