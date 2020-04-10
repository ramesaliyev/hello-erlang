# Hello Erlang!
These are notes and quotes i take about Erlang while
reading the [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) book.

# Definitions
<details>
  <summary><strong>referential transparency</strong></summary>
  An expression is called referentially transparent if it can be replaced with its corresponding value without changing the program's behavior. This requires that the expression be pure, that is to say the expression value must be the same for the same inputs and its evaluation must have no side effects. An expression that is not referentially transparent is called referentially opaque.
</details>

# Links
- [Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)

# Setup
#### install erlang
    brew install erlang
#### run shell to test it
    erl