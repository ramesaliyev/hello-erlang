# Hello Erlang!
These are notes and quotes i take about Erlang in my learning process.<br>
Check [resources](#resources) section to see my learning sources.

***

# Topics
<details>
  <summary><strong>gotchas</strong></summary><br>

- erlang has no such thing as a `null` value
- every function needs to return something
- erlang doesn't allow default arguments in functions
- erlang is built on the notion that a failure in one of the components should not affect the whole system
- every erlang term can be compared to any other
</details>

## basic data types
<details>
  <summary><strong>numbers</strong></summary><br>

both floating point numbers or integers are supported when dealing with arithmetic. Integers and floating values are pretty much the only types of data Erlang's mathematical operators will handle transparently for you. to have the integer-to-integer division, use `div`, and to have the modulo operator, use `rem`.

    5 / 2.
    -> 2.5

    5 div 2
    -> 2

to express integers in other bases than base 10, just enter the number as `Base#Value` (given `Base` is in the range `2..36`):

    2#101010.
    -> 42

    8#0677.
    -> 447

    16#AE.
    -> 174

</details>
<details>
  <summary><strong>invariable variables</strong></summary><br>

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
<details>
  <summary><strong>atoms</strong></summary><br>

atoms are literals, constants with their own name for value. an atom should be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore or @.

    atom.
    atoms_rule@erlang.
    'Atoms can be cheated!'.

an atom with single quotes is exactly the same as a similar atom without them

    atom = 'atom'.
    -> atom

an atom is referred to in an `atom table` which consumes memory. atom table is not garbage collected, and so atoms will accumulate until the system tips over, either from **memory usage** or because **1048577** atoms were declared. this means atoms should not be generated dynamically for whatever reason.

some atoms are reserved words: `after`, `and`, `andalso`, `band`, `begin`, `bnot`, `bor`, `bsl`, `bsr`, `bxor`, `case`, `catch`, `cond`, `div`, `end`, `fun`, `if`, `let`, `not`, `of`, `or`, `orelse`, `query`, `receive`, `rem`, `try`, `when`, `xor`

atoms can only be compared and nothing else
</details>
<details>
  <summary><strong>boolean algebra</strong></summary><br>

there is not much to say

    true and false.
    -> false

    false or true.
    -> true

    true xor false.
    -> true

    not false.
    -> true

    not (true and true).
    -> false

the boolean operators `and` and `or` will always evaluate arguments on both sides of the operator. If you want to have the short-circuit operators (which will only evaluate the right-side argument if it needs to), use `andalso` and `orelse`.

erlang has no such things as boolean `true` and `false`. **the terms true and false are atoms**, *but they are integrated well enough into the language you shouldn't have a problem with that as long as you don't expect false and true to mean anything but false and true.*
</details>
<details>
  <summary><strong>comparison operators</strong></summary><br>

- `=:=` exactly equal
- `=/=` exactly not equal
- `==` equal
- `/=` not equal
- `<` less than
- `>` greather than
- `>=` greater than or equal to
- `=<` less than or equal to

```
5 =:= 5.
-> true

1 =:= 0.
-> false

1 =/= 0.
-> true

5 =:= 5.0.
-> false

5 =:= true.
-> false

5 == 5.0.
-> true

5 /= 5.0.
-> false

1 < 2.
-> true

1 < 1.
-> false

1 >= 1.
-> true

1 =< 1.
-> true

5 + llama.
-> ** exception error: bad argument in an arithmetic expression
```

the correct ordering of each element in a comparison is the following: <br>
`number` **<** `atom` **<** `reference` **<** `fun` **<** `port` **<** `pid` **<** `tuple` **<** `list` **<** `bit string`

this is why you can compare anything with anything.
```
0 == false.
-> false

1 < false.
-> true
```
</details>
<details>
  <summary><strong>tuples</strong></summary><br>
A tuple is a way to organize data.

    Point = {3,5}.

    {X,Y} = Point.
    X.
    -> 3
    Y.
    -> 5

    {_,Y2} = Point.
    Y2.
    -> 5

`_` is the anonymous variable. this is exactly how it's meant to be used: to drop the value that would usually be placed there since we won't use it. The `_` variable is always seen as unbound and acts as a wildcard for pattern matching.

pattern matching to unpack tuples will only work if the number of elements (the tuple's length) is the same.

    {_,_} = {4,5,6}.
    -> ** exception error: no match of right hand side value {4,5,6}

tuples can also be useful when working with single values.

    PreciseTemperature = {celsius, 23.213}.
    {kelvin, T} = PreciseTemperature.
    -> ** exception error: no match of right hand side value {celsius,23.213}

a tuple which contains an atom with one element following it is called a `tagged tuple`.

    {point, {X,Y}}.

get `nth` element of a tuple

    element(2,{a,b,c}).
    -> b

you can do some arithmetic classifying operations on tuples such as sort, min, max, etc.

    lists:sort([{2, b}, {4, d}, {1, a}, {3, c}]).
    -> [{1,a},{2,b},{3,c},{4,d}]

    erlang:min({3, x}, {1, y}).
    -> {1, y}

    erlang:max({3, x}, {7, y}).
    -> {7, y}
</details>
<details>
  <summary><strong>lists</strong></summary><br>

lists can contain anything and you can mix more than one type of data in it.

    [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].

strings are lists and the notation is absolutely the exact same

    [97, 98, 99].
    -> "abc"

erlang will print lists of numbers as numbers only when at least one of them could not also represent a letter

    [97,98,99,4,5,6].
    -> [97,98,99,4,5,6]

    [233].
    -> "é"

there is no such thing as a real string in erlang

glue lists together

    [1,2,3] ++ [4,5].
    -> [1,2,3,4,5]

remove elements from a list

    [1,2,3,4,5] -- [1,2,3].
    -> [4,5]

    [2,4,2] -- [2,4].
    -> [2]

    [2,4,2] -- [2,4,2].
    -> []

    [2,3,4,5] -- [3,4].
    -> [2,5]

    [2,3,4,5] -- [3,5].
    -> [2,4]

both `++` and `--` are right-associative

    [1,2,3] -- [1,2] -- [3].
    -> [3]

    [1,2,3] -- [1,2] -- [2].
    -> [2,3]

get length

    length([1,2,3,4]).
    -> 4

get head

    hd([1,2,3,4]).
    -> 1

get tail

    tl([1,2,3,4]).
    -> [2,3,4]

or use pattern matching

    [Head|Tail] = [1,2,3,4].

    Head.
    -> 1

    Tail.
    -> [2,3,4]

    [First, Second | Rest] = [1,2,3,4,5].
    -> [1,2,3,4,5]
    First.
    -> 1
    Second.
    -> 2
    Rest.
    -> [3,4,5]

add head

    List = [2,3,4].
    NewList = [1|List].
    -> [1,2,3,4]

`|` is named the `cons operator` (constructor),  any list can be built with only cons and values

    [3 | [2 | [1 | []] ] ].
    -> [3,2,1]

note: using the form `[1 | 2]` gives what we call an `improper list`. improper lists will work when you pattern match in the `[Head|Tail]` manner, but will fail to be used with standard functions of erlang (even `length()`). this is because erlang expects `proper lists`. proper lists **end with an empty list** as their last cell. when declaring an item like `[2]`, the list is automatically formed in a proper manner. as such, `[1|[2]]` would work! improper lists, although syntactically valid, are of very limited use outside of user-defined data structures.

</details>
<details>
  <summary><strong>strings</strong></summary><br>

we know that strings are actually lists and there is no such thing as a real string in erlang. but we can create strings (lists) easily with double quote `"`. remember single quotes `'` are for atoms.

    String = "my string".

    is_list(String).
    -> true

but even there is no real strings, there are string operations on erlang;

    string:uppercase("hello").
    -> "HELLO"

    string:tokens("10 4 3 + 2 * -", " ").
    -> ["10","4","3","+","2","*","-"]

    string:to_float("1.3").
    -> {1.3,[]}

    list_to_integer("23").
    -> 23

    string:to_integer("23").
    -> {23,[]}

> **see [road.erl](./code/examples/road.erl) as example of string operations**

</details>
<details>
  <summary><strong>list comprehensions</strong></summary><br>

list comprehensions are ways to build or modify lists. it's based off the idea of set notation.

    [2*N || N <- [1,2,3,4]].
    -> [2,4,6,8]

    [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
    -> [2,4,6,8,10]

the `arrow` acts exactly like the `=` operator, with the exception that **it doesn't throw exceptions**.

    RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}].

    %prices of all the items costing between $3 and $10 with taxes 7%
    [{Item, Price*1.07} || {Item, Price} <- RestaurantMenu, Price >= 3, Price =< 10].

    -> [{steak,6.4}, {beer,4.3}, {poutine,3.7}]

recipe for list comprehensions in erlang is therefore; `NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]`. the part `Pattern <- List` is named a `generator expression`. you can have more than one.

    [X+Y || X <- [1,2], Y <- [2,3]].
    -> [3,4,4,5]

permutation example

    [{X,Y,Z} || X <- [1,2,3], Y <- [1,2,3], Z <- [1,2,3], X =/= Y, Y =/= Z, X =/= Z].
    -> [{1,2,3}, {1,3,2}, {2,1,3}, {2,3,1}, {3,1,2}, {3,2,1}]

more generic recipe would be; `NewList = [Expression || GeneratorExp1, GeneratorExp2, ..., GeneratorExpN, Condition1, Condition2, ... ConditionM]`

expressions coupled with pattern matching also act as a filter:

    Weather = [{toronto, rain}, {montreal, storms}, {london, fog},{paris, sun}, {boston, fog}, {vancouver, snow}].

    FoggyPlaces = [X || {X, fog} <- Weather].

    -> [london,boston]
</details>
<details>
  <summary><strong>bit syntax</strong></summary><br>

bit syntax encloses binary data between `<<` and `>>`, splits it in readable segments, and each segment is separated by a comma. a segment is a sequence of bits of a binary (not necessarily on a byte boundary, although this is the default behaviour)

    Color = 16#F09A29.
    -> 15768105

    Pixel = <<Color:24>>.
    -> <<240,154,41>>

this basically says **put the binary values of `#F09A29` on `24 bits` of space (red on 8 bits, green on 8 bits and blue also on 8 bits) in the variable Pixel.**

    Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.

    % tell erlang that each variable on the left side will hold 24 bits of data
    <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.

    % then take the first pixel and unpack it further into single color values
    <<R:8, G:8, B:8>> = <<Pix1:24>>.
    -> <<213,45,132>>

    % take only the first color from the start
    <<R:8, Rest/binary>> = Pixels.
    R.
    -> 213

more than one way to describe a binary segment is accepted. those are all valid:

- `Value`
- `Value:Size`
- `Value/TypeSpecifierList`
- `Value:Size/TypeSpecifierList`

`Size` is going to represent bits or bytes (depending on `Type` and `Unit` below), and `TypeSpecifierList` represents one or more of the following:

- **Type**
  - possible values: `integer` | `float` | `binary` | `bytes` | `bitstring` | `bits` | `utf8` | `utf16` | `utf32`
  - represents the kind of binary data used
  - `bytes` is shorthand for `binary` and `bits` is shorthand for `bitstring`
  - default: `integer`
- **Signedness**
  - possible values: `signed` | `unsigned`
  - default: `unsigned`
- **Endianness**
  - possible values: `big` | `little` | `native`
  - only matters when the Type is either `integer`, `utf16`, `utf32`, or `float`
  - this has to do with **how the system reads binary data**. as an example, the BMP image header format holds the size of its file as an integer stored on `4 bytes`. for a file that has a size of `72` bytes, a `little-endian` system would represent this as `<<72,0,0,0>>` and a `big-endian` one as `<<0,0,0,72>>`. one will be read as `72` while the other will be read as `1207959552`, so make sure you use the right endianness. there is also the option to use `native`, which will choose at run-time if the CPU uses little-endianness or big-endianness natively.
  - default: `big`
- **Unit**
  - written `unit:Integer`
  - the size of each segment, in bits
  - allowed range is `1..256`
  - default is 1 for `integers`, `floats` and `bitstrings` (`bits`)
  - default is 8 for `binary` (`bytes`)
  - `utf8`, `utf16` and `utf32` types require no unit to be defined
  - the multiplication of `Size` by `Unit` is equal to the number of bits the segment will take and must be evenly divisible by `8`. the unit size is usually used to ensure byte-alignment.

`TypeSpecifierList` is built by separating attributes by a `-`

    <<X1/unsigned>> = <<-44>>.
    X1.
    -> 212

    <<X2/signed>> = <<-44>>.
    X2.
    -> -44

    <<X2/integer-signed-little>> = <<-44>>.
    X2.
    -> -44

    <<N:8/unit:1>> = <<72>>.
    N.
    -> 72

    <<N/integer>> = <<72>>.
    N.
    -> 72

    <<Y:4/little-unit:8>> = <<72,0,0,0>>.
    Y.
    -> 72

binary operations:

- `bsl` bit shift left
- `bsr` bit shift right
- `band` and
- `bor` or
- `bxor` xor
- `bnot` not

operators could be used as infix operators

    2#00100 = 2#00010 bsl 1.
    2#00001 = 2#00010 bsr 1.
    2#10101 = 2#10001 bor 2#00101.

parsing TCP segments example:

    <<SourcePort:16, DestinationPort:16, AckNumber:32,
    DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
    CheckSum: 16, UrgentPointer:16, Payload/binary>> = SomeBinary.

**bit strings:** more efficient in terms of space, because normal lists are linked lists while bit strings are more like C arrays. downside of binary strings compared to lists is a loss in simplicity when it comes to pattern matching and manipulation. people tend to use binary strings when storing text that won't be manipulated too much or when space efficiency is a real issue. dont use strings instead of atoms, or atoms instead of strings. strings can be manipulated (splitting, regular expressions, etc) while atoms can only be compared and nothing else.

    Bitstring = <<"this is a bit string!">>.
    Bitstring.
    -> <<"this is a bit string!">>
</details>
<details>
  <summary><strong>binary comprehensions</strong></summary><br>

binary comprehensions are to bit syntax what list comprehensions are to lists: a way to make code short and concise

    [X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0].

only change in syntax from regular list comprehensions is the `<-` which became `<=` and using `binaries` `<<>>` instead of `lists` `[]`

    Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
    RGB = [{R,G,B} || <<R:8,G:8,B:8>> <= Pixels].
    RGB.
    -> [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

changing `<-` to `<=` let us use a binary stream as a generator

binary comprehension syntax to change non-binary data to binary data;

    << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.

it is possible to have a binary comprehension with a binary generator

    << <<(X+1)/integer>> || <<X>> <= <<3,7,5,4,7>> >>.
</details>

## modules
<details>
  <summary><strong>modules</strong></summary><br>

- modules are a bunch of `functions` regrouped in a single file, under a single name.
- all functions in erlang must be defined in modules.
- modules need to be called in form of `Module:Function(Arguments)`
- you can declare two kinds of things in a module: `functions` and `attributes`

check [hello.erl](./code/hello.erl) for first module example

## attributes

all module attributes follow the form `-Name(Attribute).` <br>
attributes are metadata describing the module itself such as its name, the functions that should be visible to the outside world, the author of the code, and so on.

`-module(Name)` <br>
necessary for your module to be compilable: name of the current module. this is the name you'll use to call functions from other modules. `Name` is an `atom`.

`-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).` <br>
defines what functions of a module can be called by the outside world.

`-import(Module, [Function1/Arity, ..., FunctionN/Arity]).` <br>
erlang programmers are often discouraged from using this attribute. leaving the module name in is considered good practice.

`-vsn(VersionNumber).` <br>
unique value differentiating each version of your code. this will be generated automatically if you dont specify. see the compiling topic.

`-author("Sterling Archer").` <br>
author info

- **the arity of a function is an integer representing how many arguments can be passed to the function.**
- different functions defined within a module can share the same name if and only if they have a different arity.
- the functions `add(X,Y)` and `add(X,Y,Z)` would thus be considered different and written in the form `add/2` and `add/3` respectively.

## functions

a function follows the form `Name(Args) -> Body.`<br>
- `Name` has to be an `atom` and `Body` can be **one or more** erlang `expressions` separated by commas.
- the function is ended with a period.
- last logical expression of a function to be executed will have its value returned to the caller
- functions and expressions **must always return something**, if they wont, they will crash

## macros

a macro is defined as a module attribute of the form:`-define(MACRO, some_value).` <br>
and is used as `?MACRO` inside any function defined in the module.

a function macro example

    % define
    -define(sub(X,Y), X-Y).

    % usage
    ?sub(23,47).

## comments
comments are single-line only and begin with a `%` sign (using `%%` is purely a question of style.)

## module design
- avoid circular dependencies!
- it is usually considered a good practice to regroup functions that have similar roles close together

</details>
<details>
  <summary><strong>compiling and using the code</strong></summary><br>

erlang code is compiled to `bytecode` in order to be used by the `virtual machine`.
- from command line: `erlc flags file.erl`
- from shell or in module `compile:file(FileName)`
- from shell `c(FileName)`
  - c("code/function_syntax/pattern").

## using without compiling the code

[`escript`](http://erlang.org/doc/man/escript.html) provides support for running short Erlang programs without having to compile them first, and an easy way to retrieve the command-line arguments.

## compiling from command line

    $ erlc code/examples/road.erl
    -> code/examples/road.erl:2: Warning: export_all flag enabled - all functions will be exported

    $ erl -noshell -run road main code/examples/road.txt
    -> [{b,10},{x,30},{a,5},{x,20},{b,2},{b,8}]

## compiling code from shell

`erl`

    cd("/path/to/where/you/saved/the-module/").
    -> ok

    c(hello).
    -> {ok,hello}

    % or

    compile:file(hello).
    -> {ok,hello}

    % compiling with some flags, see below for flags

    c(hello, [debug_info, export_all]).
    -> {ok,hello}

    % or

    compile:file(hello, [debug_info, export_all]).
    -> {ok,hello}

### usage in shell after compiling from shell

after code is compiled, a `hello.beam` file will be added next to `hello.erl` in your directory. this is the compiled module.
`.beam` stands for `Bogdan/Björn's Erlang Abstract Machine`, which is the VM itself.

let's try our module:

    hello:add(7,2).
    -> 9

    hello:hello().
    -> Hello, world!
    -> ok % io:format/1 returns 'ok' to denote a normal condition, the absence of errors.

    hello:greet_and_add_two(7).
    -> Hello, world!
    -> 9

    hello:print_macroex().
    -> 7

## compilation flags
the most common flags are:

`-debug_info`<br>
erlang tools such as debuggers, code coverage and static analysis tools will use the debug information of a module in order to do their work.

`-{outdir,Dir}`<br>
by default, the erlang compiler will create the `beam` files in the current directory. this will let you choose where to put the compiled file.

`-export_all`<br>
will ignore the `-export` module attribute and will instead export all functions defined. useful for testing.

`-{d,Macro}` or `{d,Macro,Value}`
defines a macro to be used in the module, where `Macro` is an atom. more frequently used when dealing with `unit-testing`, ensuring that a module will only have its testing functions created and exported when they are explicitly wanted. by default, `Value` is `true` if it's not defined as the third element of the tuple.

[list of all of them](http://erlang.org/doc/man/compile.html)

compilation flags can be defined from within a module;

    -compile([debug_info, export_all]).

## compiling to native code

native code compiling is not available for every platform and OS, but on those that support it, it can make your programs go faster (about 20% faster, based on anecdotal evidence). to compile to native code, you need to use the `hipe` module and call it the following way: `hipe:c(Module,OptionsList).` you could also use `c(Module,[native]).` when in the shell to achieve similar results. Note that the `.beam` file generated will contain both native and non-native code, and the native part will not be portable across platforms.

## accesing metadata of a module
compiler will pick up most module attributes and store them (along with other information) in a `module_info/0` function.

    hello:module_info().
    -> [{module,hello},
        {exports,[
          {hello,0},
          {add,2},
          {greet_and_add_two,1},
          {print_macroex,0},
          {module_info,0},
          {module_info,1}]},
        {attributes,[
          {vsn,[77148074631179461122195354627063078466]},
          {author,"Sterling Archer"}]},
        {compile,[
          {version,"7.5.4"},
          {options,[debug_info,export_all]},
          {source,"/Users/ramesaliyev/Projects/Personal/hello-erlang/code/hello.erl"}]},
        {native,false},
        {md5,<<58,10,45,191,213,113,184,164,243,212,168,133,38,26,222,66>>}
      ]

`module_info/1` will let you grab one specific piece of information.

    hello:module_info(md5).
    -> <<58,10,45,191,213,113,184,164,243,212,168,133,38,26,222,66>>

`vsn` is an automatically generated `unique value differentiating` each version of your code, excluding comments. it is used in `code hot-loading` (upgrading an application while it runs, without stopping it) and by some tools related to release handling. You can also specify a vsn value yourself if you want: just add `-vsn(VersionNumber)` to your module.

an [example usage of module attributes  in a testing script](https://learnyousomeerlang.com/static/erlang/tester.erl) to annotate functions for which unit tests could be better; the script looks up module attributes, finds the annotated functions and shows a warning about them.
</details>
<details>
  <summary><strong>io:format</strong></summary><br>
- `io:format`'s formatting is done with the help of tokens being replaced in a string
- the character used to denote a token is the tilde (`~`)
- some tokens are built-in such as `~n`, which will be changed to a `line-break`
- `~s` accepts strings and bitstrings as arguments
- `~p` will print an erlang term with indentation etc in a nice way

    io:format("~s!~n",["Hello"]).
    -> "Hello!\n"
    -> ok

    io:format("~p~n",[<<"Hello">>]).
    -> <<"Hello">>
    -> ok

    io:format("~~~n").
    -> ~
    -> ok

    io:format("~f~n", [4.0]).
    -> 4.000000
    -> ok

    io:format("~30f~n", [4.0]).
    ->                      4.000000
    -> ok

[more about io:format](http://erlang.org/doc/man/io.html#format-3)
</details>

## syntax in functions
<details>
  <summary><strong>pattern matching</strong></summary><br>

  pattern matching cannot express things like a range of value or certain types of data. we're gonna use guards for that. pattern matching good for specifying really precise values or abstract values.

  when pattern matching, the code we had written didn't have to know what it would be matched against. The tuple `{X,Y}` could be matched with `{atom, 123}` as well as `{"A string", <<"binary stuff!">>}, {2.0, ["strings","and",atoms]}` or really anything at all.

> **see [pattern.erl](./code/function_syntax/pattern.erl)**

    pattern:greet(male, "Sterling").
    -> Hello, Mr. Sterling!
    -> ok

    pattern:greet(female, "Lana").
    -> Hello, Mrs. Lana!
    -> ok

    pattern:greet(unknown, "Ray").
    -> Hello, Ray!
    -> ok

each of these function declarations is called a `function clause`. function clauses must be separated by semicolons (`;`) and together form a `function declaration`. a function declaration counts as one larger statement, and it's why the final function clause ends with a period.

`head/1`, `second/1`;

    -> pattern:head([1,2,3,4]).
    -> 1

    -> pattern:second([1,2,3,4]).
    -> 2

`same/2`

    pattern:same(1,1).
    -> true

    pattern:same(1,4).
    -> false

    pattern:same(cat,dog).
    -> false

    pattern:same(cat,cat).
    -> true

when you call `same(a,a)` is that the first `X` is seen as **unbound**: it automatically takes the value `a`. then when Erlang goes over to the `second argument`, it sees `X` is **already bound**. it then compares it to the `a` passed as the second argument and looks to see if it matches. **the pattern matching succeeds and the function returns true**. if the two values aren't the same, this will fail and go to the second function clause, which doesn't care about its arguments and will instead return **false**.

`valid_time/1`

it is possible to use the `=` operator in the function head, allowing us to match both the content inside a tuple (`{Y,M,D}`) and the tuple as a whole (`Date`)

    pattern:valid_time({{2011,09,06}, {09,04,43}}).
    -> The Date tuple ({2011,9,6}) says today is: 2011/9/6,
    -> The time tuple ({9,4,43}) indicates: 9:4:43.
    -> ok

    pattern:valid_time({{2011,09,06},{09,04}}).
    -> Stop feeding me wrong data!
    -> ok

there is a problem though! this function could take anything for values, even text or atoms, as long as the tuples are of the form `{{A,B,C}, {D,E,F}}`. this denotes **one of the limits of pattern matching: it can either specify really precise values** such as a known number of atom, **or abstract values** such as the head|tail of a list, a tuple of N elements, or anything (`_` and unbound variables), etc. **to solve this problem, we use guards.**

## bound and unbound variables

**unbound** variables are variables without any values attached to them. binding a variable is simply attaching a value to an unbound variable. in the case of erlang, **when you want to assign a value to a variable that is already bound, an error occurs unless the new value is the same as the old one**.
</details>
<details>
  <summary><strong>guards</strong></summary><br>

guards are additional clauses that can go in a function's head to make pattern matching more expressive. can express a range of value or certain types of data.

> **see [guards.erl](./code/function_syntax/guards.erl)**

    guards:old_enough(15).
    -> false

    guards:old_enough(17).
    -> true

    guards:right_age(17).
    -> true

    guards:right_age(106).
    -> false

    guards:wrong_age(17).
    -> false

    guards:wrong_age(106).
    -> true

basic rule for guard expression is they must **return `true`** to succeed. will fail if it **`returns false`** or if it **`throws an exception`**.

in guards, the comma (`,`) acts in a similar manner to the operator `andalso` and the semicolon (`;`) acts a bit like `orelse`. but they're not exactly the same. The comma and semicolon pair will catch exceptions as they happen while the `andalso` and `orelse` won't. what this means is that if there is an error thrown in the first part of the guard `X >= N; N >= 0`, the second part can still be evaluated and the guard might succeed; if an error was thrown in the first part of `X >= N orelse N >= 0`, the second part will also be skipped and the whole guard will fail. if the first guard fails, it then tries the
second, and then the next one, until either one guard succeeds or they all fail.

only `andalso` and `orelse` can be nested inside guards. this means `(A orelse B) andalso C` is a valid guard, while `(A; B), C` is not. given their different use, the best strategy is often to mix them as necessary.

    guards:is_okay(true, false, false).
    -> false

    guards:is_okay(false, false, true).
    -> false

    guards:is_okay(false, true, true).
    -> true

math operations and functions about data types, such as `is_integer/1`, `is_atom/1`, etc. can be used inside guard expressions.

    guards:is_square(3, 9).
    -> true

    guards:is_square(3, 93).
    -> false

    guards:is_between(17, 9, 31).
    -> true

    guards:is_between("Hello", 9, 31).
    -> false

**list of available functions;**

type checking functions: `is_atom/1`, `is_binary/1`, `is_bitstring/1`, `is_boolean/1`, `is_builtin/3`, `is_float/1`, `is_function/1`, `is_function/2`, `is_integer/1`, `is_list/1`, `is_number/1`, `is_pid/1`, `is_port/1`, `is_record/2`, `is_record/3`, `is_reference/1`, `is_tuple/1`

other allowed functions: `abs(Number)`, `bit_size(Bitstring)`, `byte_size(Bitstring)`, `element(N`, `Tuple)`, `float(Term)`, `hd(List)`, `length(List)`, `node()`, `node(Pid|Ref|Port)`, `round(Number)`, `self()`, `size(Tuple|Bitstring)`, `tl(List)`, `trunc(Number)`, `tuple_size(Tuple)`

but guard expressions **will not accept user-defined functions** because of side effects. erlang is not a purely functional programming language (like `Haskell` is) because it relies on side effects a lot: you can do I/O, send messages between actors or throw errors as you want and when you want. there is no trivial way to determine if a function you would use in a guard would or wouldn't print text or catch important errors every time it is tested over many function clauses. so instead, erlang just doesn't trust you.

when erlang can't find a way to have a guard succeed, it will crash: it **cannot not return something.**
</details>
<details>
  <summary><strong>if</strong></summary><br>

`if`s act like guards and share guards' syntax, but outside of a function clause's head. the if clauses are called `Guard Patterns`.

> **see [ifs.erl](./code/function_syntax/ifs.erl)**

when erlang can't find a way to have a guard succeed, it will crash: it **cannot not return something.** because of that we need to add a catch-all branch that will always succeed no matter what. in most languages, this would be called an `else`. in Erlang, we use `true`

    ifs:is_two(1).
    -> no

    ifs:is_two(2).
    -> yes

    ifs:print_me(male, 30, "Sterling").
    -> Hello Sterling! You are a boy! You are NOT a teenager!ok

    ifs:print_me(female, 17, "Lana").
    -> Hello Lana! You are a girl! You are a teenager!ok

    ifs:animal_says(dog).
    -> {dog,"says bark!"}

    ifs:animal_says(beef).
    -> {beef,"says mooo!"}

    ifs:animal_says(zombie).
    -> {zombie,"says idunnowhattosay!"}

    ifs:animal_says("zombie").
    -> {"zombie","says idunnowhattosay!"}
</details>
<details>
  <summary><strong>case ... of</strong></summary><br>

if the `if` expression is like a `guard`, a `case ... of` expression is like the whole function head: you can have the complex pattern matching you can use with each argument, and you can have guards on top of it!

> **see [cases.erl](./code/function_syntax/cases.erl)**

    cases:insert(archer, []).
    -> [archer]

    cases:insert(bender, [archer]).
    -> [bender, archer]

    cases:insert(bender, [archer, bender]).
    -> [archer, bender]

</details>
<details>
  <summary><strong>which to use</strong></summary><br>

even the writer of the book not sure what to say about `function heads` vs `case ... ofs`. and the community is not agreed either.

`if` was added to the language as a short way to have guards without needing to write the whole pattern matching part when it wasn't needed.

all of this is more about personal preferences and what you may encounter more often. there is no good solid answer.
</details>

## types
<details>
  <summary><strong>dynamically and strongly typed</strong></summary><br>

**erlang is dynamically typed**: every error is caught at **runtime** and the compiler won't always yell at you when compiling modules where things may result in failure.

**erlang is also strongly typed**: it wont do implicit type conversions between terms.
</details>
<details>
  <summary><strong>type conversions</strong></summary><br>

each of casting functions take the form `<type>_to_<type>` and are implemented in the `erlang` module.

    erlang:list_to_integer("54").
    -> 54

    erlang:integer_to_list(54).
    -> "54"

    erlang:list_to_float("54.32").
    -> 54.32

    erlang:atom_to_list(true).
    -> "true"

    erlang:list_to_atom("true").
    -> true

    erlang:list_to_bitstring("hi there").
    -> <<"hi there">>

    erlang:bitstring_to_list(<<"hi there">>).
    -> "hi there"

whole list:

`atom_to_binary/2`, `atom_to_list/1`, `binary_to_atom/2`, `binary_to_existing_atom/2`, `binary_to_list/1`, `bitstring_to_list/1`, `binary_to_term/1`, `binary_to_term/2`, `float_to_list/1`, `fun_to_list/1`, `integer_to_list/1`, `integer_to_list/2`, `iolist_to_binary/1`, `iolist_to_atom/1`, `list_to_atom/1`, `list_to_binary/1`, `list_to_bitstring/1`, `list_to_existing_atom/1`, `list_to_float/1`, `list_to_integer/2`, `list_to_pid/1`, `list_to_tuple/1`, `pid_to_list/1`, `port_to_list/1`, `ref_to_list/1`, `term_to_binary/1`, `term_to_binary/2` and `tuple_to_list/1`
</details>
<details>
  <summary><strong>type checking</strong></summary><br>

`is_atom/1`, `is_binary/1`, `is_bitstring/1`, `is_boolean/1`, `is_builtin/3`, `is_float/1`, `is_function/1`, `is_function/2`, `is_integer/1`, `is_list/1`, `is_number/1`, `is_pid/1`, `is_port/1`, `is_record/2`, `is_record/3`, `is_reference/1`, `is_tuple/1`
</details>

## recursion
<details>
  <summary><strong>recursion</strong></summary><br>

> **see [recursive.erl](./code/recursion/recursive.erl)**

    recursive:fac(3).
    -> 6

    recursive:len([]).
    -> 0

    recursive:len([1,2,3]).
    -> 3

    recursive:duplicate(3, x).
    -> [x,x,x]

    recursive:reverse([1,2,3]).
    -> [3,2,1]

    recursive:sublist([a,b,c,d], 2).
    -> [a,b]

    recursive:zip([a,b,c], [1,2,3]).
    -> [{a,1},{b,2},{c,3}]

    recursive:zip([a,b,c,d], [1,2,3]).
    -> ** exception error: no function clause matching

    recursive:lenient_zip([a,b,c,d], [1,2,3]).
    -> [{a,1},{b,2},{c,3}]

> for more advanced examples also see
> - [quicksort.erl](./code/examples/quicksort.erl)
> - [tree.erl](./code/examples/tree.erl)

</details>
<details>
  <summary><strong>tail recursion</strong></summary><br>

> **see [recursive.erl](./code/recursion/recursive.erl)**

tail recursion is a way to transform the linear process (it grows as much as there are call stacks) to an iterative one (there is not really any growth). to have a function call being tail recursive, it needs to be *alone*.

check *tail_* prefixed functions to see how.

tail recursion as seen here is not making the memory grow because when the virtual machine sees a function calling itself in a tail position (the last expression to be evaluated in a function), it eliminates the current stack frame. This is called `tail-call optimisation` (`TCO`) and it is a special case of a more general optimisation named `Last Call Optimisation` (`LCO`).

`LCO` is done whenever the last expression to be evaluated in a function body is another function call. when that happens, as with `TCO`, the Erlang VM avoids storing the stack frame. as such tail recursion is also possible between multiple functions. As an example, the chain of functions `a() -> b(). b() -> c(). c() -> a().` will effectively create an infinite loop that won't go out of memory as `LCO` avoids overflowing the stack. this principle, combined with our use of accumulators is what makes tail recursion useful.

the areas which tail recursion is become more important are in functions that are supposed to loop infinitely, like main loops.

    recursive:tail_fac(3).
    -> 6

    recursive:tail_len([1,2,3,4]).
    -> 4

    recursive:tail_duplicate(3, x).
    -> [x,x,x]

    recursive:tail_reverse([a,b,c]).
    -> [c,b,a]

    recursive:tail_sublist([a,b,c,d], 2).
    -> [a,b]

    recursive:tail_zip([a,b,c], [1,2,3]).
    -> [{a,1},{b,2},{c,3}]

    recursive:tail_lenient_zip([a,b,c,d], [1,2,3]).
    -> [{a,1},{b,2},{c,3}]

most of the times, as a last step, (before returning them), we will reverse our lists because they were built in a tail-recursive manner. examples in recursive.erl doesnt follow this pratic. see advanced examples.

</details>

## higher order functions
<details>
  <summary><strong>higher order functions</strong></summary><br>

function that can accept other functions transported around is named a higher order function.

> **see [hof.erl](./code/higher_order_fns/hof.erl)**

    hof:add(fun hof:one/0, fun hof:two/0).

`fun Module:Function/Arity` tells the VM to use that specific function, and then bind it to a variable.

if function names are written without a parameter list then those names are interpreted as atoms, and atoms can not be functions, so the call fails.

    hof:map(fun hof:incr/1, [1,2,3,4,5]).
    -> [2,3,4,5,6]

    hof:map(fun hof:decr/1, [1,2,3,4,5]).
    -> [0,1,2,3,4]

</details>
<details>
  <summary><strong>anonymous functions</strong></summary><br>

anonymous functions, or `fun`s, letting you declare a special kind of function inline, without naming it. they can do pretty much everything normal functions can do, except calling themselves recursively.

    fun(Args1) ->
      Expression1, Exp2, ..., ExpN;
    (Args2) ->
      Expression1, Exp2, ..., ExpN;
    (Args3) ->
      Expression1, Exp2, ..., ExpN
    end

example

    Fn = fun() -> a end.
    -> #Fun<erl_eval.21.126501267>
    Fn().
    -> a

> **see [hof.erl](./code/higher_order_fns/hof.erl)**

    hof:map(fun(N) -> N*5 end, [1,2,3,4,5]).
    -> [5,10,15,20,25]

## closures

    PrepareAlarm =
      fun(Room) -> io:format("Alarm set in ~s.~n",[Room]),
        fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
      end.

    -> #Fun<erl_eval.20.67289768>

    AlarmReady = PrepareAlarm("bathroom").
    -> Alarm set in bathroom.
    -> #Fun<erl_eval.6.13229925>

    AlarmReady().
    -> Alarm tripped in bathroom! Call Batman!
    -> ok

> **see [anonymous.erl](./code/higher_order_fns/anonymous.erl)**

    anonymous:b(anonymous:a()).
    -> "a/0's password is pony"

    Base = 2.
    PowerOfBase = fun(X) -> math:pow(Base,X) end.
    hof:map(PowerOfBase, [1,2,3,4,5]).
    -> [2.0, 4.0, 8.0, 16.0, 32.0]

</details>
<details>
  <summary><strong>anonymous but named functions</strong></summary><br>

the language supports using anonymous functions with an internal name. the trick is that the name is visible only within the function's scope, not outside of it. main advantage of this is that it makes it possible to define anonymous recursive functions.

    PrepareAlarm = fun(Room) ->
      io:format("Alarm set in ~s.~n",[Room]),
      fun Loop() ->
        io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
        timer:sleep(500),
        Loop()
      end
    end.
    -> #Fun<erl_eval.7.126501267>

    AlarmReady = PrepareAlarm("bathroom").
    -> Alarm set in bathroom.
    -> #Fun<erl_eval.45.126501267>

    AlarmReady().
    -> Alarm tripped in bathroom! Call Batman!
    -> Alarm tripped in bathroom! Call Batman!
    -> Alarm tripped in bathroom! Call Batman!
    -> ...

</details>
<details>
  <summary><strong>maps, filters, folds and more</strong></summary><br>

> **see [fold.erl](./code/higher_order_fns/fold.erl)**

    % MAP

    fold:map(fun(N) -> N/2 end, [2,4,6,8,10]).
    -> [1.0,2.0,3.0,4.0,5.0]

    % FILTER

    fold:even([1,2,3,4,5]).
    -> [2,4]

    fold:old_men([{male, 80}, {male, 30}, {female, 65}, {male, 70}]).
    -> [{male,80},{male,70}]

    Numbers = lists:seq(1,10).
    -> [1,2,3,4,5,6,7,8,9,10]
    fold:filter(fun(X) -> X rem 2 == 0 end, Numbers).
    -> [2,4,6,8,10]

    People = [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}].
    fold:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).
    -> [{male,66},{male,74}]

    % FOLD

    fold:max([5,6,3,1,9,2]).
    -> 9

    fold:min([5,6,3,1,9,2]).
    -> 1

    fold:sum([1,2,3,4,5]).
    -> 15

    % any function you can think of that reduces lists
    % to 1 element can be expressed as a fold

    NList = [1,7,3,5,9,0,2,3,-5].
    [H|T] = NList.

    % max
    fold:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
    -> 9

    % min
    fold:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
    -> -5

    % sum
    fold:fold(fun(A,B) -> A + B end, 0, NList).
    -> 25

additionally you can represent an accumulator as a single element (or a single variable), and an accumulator can be a list. therefore, we can use a fold to build a list. this means **fold is universal in the sense that you can implement pretty much any other recursive function on lists with a fold, even map and filter**:

    % fold
    fold(_, Start, []) -> Start;
    fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

    % reverse
    freverse(L) -> fold(fun(X,Acc) -> [X|Acc] end, [], L).

    % map
    fmap(F,L) -> freverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

    % filter
    ffilter(Pred, L) ->
      F = fun(X,Acc) ->
        case Pred(X) of
          true  -> [X|Acc];
          false -> Acc
        end
      end,
      freverse(fold(F, [], L)).

    % examples
    fold:freverse([1,2,3]).
    -> [3,2,1]

    fold:fmap(fun (A) -> A * 10 end, [1,2,3]).
    -> [10,20,30]

    fold:ffilter(fun (A) -> A > 17 end, [1,4,7,13,54,133]).
    -> [54,33]

`map`, `filter`s and `fold`s are only one of many abstractions over lists provided by the erlang standard library (see `lists:map/2`, `lists:filter/2`, `lists:foldl/3` and `lists:foldr/3`).

check lists documentations, [official](http://erlang.org/doc/man/lists.html) or [erldocs](https://erldocs.com/maint/stdlib/lists.html), to see all list functions.

</details>

## errors and exceptions
<details>
  <summary><strong>compile-time errors</strong></summary><br>

compile-time errors are often syntactic mistakes. here's a list of some of the common compile-time error messages and potential resolutions in case you encounter them:

**`module.beam: Module name 'madule' does not match file name 'module'`**<br>
the module name you've entered in the -module attribute doesn't match the filename

**`./module.erl:2: Warning: function some_function/0 is unused`**<br>
you have not exported a function, or the place where it's used has the wrong name or arity. it's also possible that you've written a function that is no longer needed.

**`./module.erl:2: function some_function/1 undefined`**<br>
the function does not exist. You've written the wrong name or arity either in the -export attribute or when declaring the function. this error is also output when the given function could not be compiled, usually because of a syntax error like forgetting to end a function with a period.

**`./module.erl:5: syntax error before: 'SomeCharacterOrWord'`**<br>
this happens for a variety of reason, namely unclosed parentheses, tuples or wrong expression termination (like closing the last branch of a case with a comma). other reasons might include the use of a reserved atom in your code or unicode characters getting weirdly converted between different encodings.

**`./module.erl:5: syntax error before:`**<br>
this usually comes up when your line termination is not correct. This is a specific case of the previous error, so just keep an eye out.

**`./module.erl:5: Warning: this expression will fail with a 'badarith' exception`**<br>
erlang is all about dynamic typing, but remember that the types are strong. In this case, the compiler is smart enough to find that one of your arithmetic expressions will fail (say, `llama + 5`). it won't find type errors much more complex than that, though.

**`./module.erl:5: Warning: variable 'Var' is unused`**<br>
you declared a variable and never use it afterwards. this might be a bug with your code, so double-check what you have written. otherwise, you might want to switch the variable name to `_` or just prefix it with an underscore (something like `_Var`) if you feel the name helps make the code readable.

**`./module.erl:5: Warning: a term is constructed, but never used`**<br>
in one of your functions, you're doing something such as building a list, declaring a tuple or an anonymous function without ever binding it to a variable or returning it. this warning tells you you're doing something useless or that you have made some mistake.

**`./module.erl:5: head mismatch`**<br>
it's possible your function has more than one head, and each of them has a different arity. don't forget that different arity means different functions, and you can't interleave function declarations that way. this error is also raised when you insert a function definition between the head clauses of another function.

**`./module.erl:5: Warning: this clause cannot match because a previous clause at line 4 always matches`**<br>
a function defined in the module has a specific clause defined after a catch-all one. as such, the compiler can warn you that you'll never even need to go to the other branch.

**`./module.erl:9: variable 'A' unsafe in 'case' (line 5)`**<br>
you're using a variable declared within one of the branches of a `case ... of` outside of it. this is considered `unsafe`. if you want to use such variables, you'd be better of doing `MyVar = case ... of...`
</details>
<details>
  <summary><strong>run-time errors</strong></summary><br>

list of common run-time errors with an explanation and example code that could generate them.

**`function_clause`**<br>
all the guard clauses of a function failed, or none of the function clauses' patterns matched.

    lists:sort([3,2,1]).
    -> [1,2,3]

    lists:sort(fffffff).
    -> ** exception error: no function clause matching lists:sort(fffffff)

**`case_clause`**<br>
looks like someone has forgotten a specific pattern in their case, sent in the wrong kind of data, or needed a catch-all clause!

    case "Unexpected Value" of
       expected_value -> ok;
       other_expected_value -> 'also ok'
    end.
    -> ** exception error: no case clause matching "Unexpected Value"

**`if_clause`**<br>
this is pretty similar to `case_clause` errors: it can not find a branch that evaluates to `true`. ensuring you consider all cases or add the `catch-all true` clause might be what you need.

    if 2 > 4 -> ok;
      0 > 1 -> ok
    end.
    -> ** exception error: no true branch found when evaluating an if expression

**`badmatch`**<br>
badmatch errors happen whenever pattern matching fails. this most likely means you're trying to do impossible pattern matches, trying to bind a variable for the second time, or just anything that isn't equal on both sides of the `=` operator (which is pretty much what makes rebinding a variable fail!). note that this error sometimes happens because the programmer believes that a variable of the form `_MyVar` is the same as `_`. variables with an underscore are normal variables, except the compiler won't complain if they're not used. it is not possible to bind them more than once.

    [X,Y] = {4,5}.
    -> ** exception error: no match of right hand side value {4,5}

**`badarg`**<br>
this one is really similar to `function_clause` as it's about calling functions with incorrect arguments. the main difference here is that this error is usually triggered by the programmer after validating the arguments from within the function, outside of the guard clauses. we will see how to throw such errors later in this chapter.

    erlang:binary_to_list("heh, already a list").
    -> ** exception error: bad argument
    ->     in function  binary_to_list/1
    ->        called as binary_to_list("heh, already a list")

**`undef`**<br>
this happens when you call a function that doesn't exist. another reason to get the message is when the module is not in Erlang's search path. by default, Erlang's search path is set to be in the `current directory`. you can add paths by using `code:add_patha/1` or `code:add_pathz/1`.

    lists:random([1,2,3]).
    -> ** exception error: undefined function lists:random/1

**`badarith`**<br>
this happens when you try to do arithmetic that doesn't exist, like divisions by zero or between atoms and numbers.

    5 + llama.
    -> ** exception error: bad argument in an arithmetic expression

**`badfun`**<br>
the most frequent reason why this error occurs is when you use variables as functions, but the variable's value is not a function.

    hof:add(one, two).
    -> ** exception error: bad function one

**`badarity`**<br>
the badarity error is a specific case of `badfun`: it happens when you use higher order functions, but you pass them more (or fewer) arguments than they can handle.

    F = fun(_) -> ok end.
    F(a,b).
    -> ** exception error: interpreted function with arity 1 called with two arguments

**`system_limit`**<br>
there are many reasons why a `system_limit` error can be thrown:
- too many processes (we'll get there),
- atoms that are too long,
- too many arguments in a function,
- number of atoms too large,
- too many nodes connected,
- etc,

to get a full list in details, read the [Erlang Efficiency Guide](http://erlang.org/doc/efficiency_guide/advanced.html#id2265856) on system limits. note that some of these errors are serious enough to crash the whole VM.
</details>
<details>
  <summary><strong>raising exceptions</strong></summary><br>

in trying to monitor the execution of code and protect against `logical errors`, it's often a good idea to provoke run-time crashes so problems will be spotted early.

there are three kinds of exceptions in Erlang: `errors`, `exits`, and `throws`. they all have different uses.

## errors
calling `erlang:error(Reason)` will end the execution in the current process and include a stack trace of the last functions called with their arguments when you catch it. these are the kind of exceptions that provoke the run-time errors above.

errors are the means for a function to stop its execution when you can't expect the calling code to handle what just happened.

example: if you get an `if_clause` error, what can you do? Change the code and recompile, that's what you can do (other than just displaying a pretty error message).

errors aren't limited to the examples above. you can define your own kind of errors too:

    erlang:error(badarith).
    -> ** exception error: bad argument in an arithmetic expression

    erlang:error(custom_error).
    -> ** exception error: custom_error

here, `custom_error` is not recognized by the Erlang shell and it has no custom translation such as "bad argument in ...", but it's usable in the same way and can be handled by the programmer in an identical manner (we'll see how to do that soon).

## exits
there are two kinds of exits: `internal exits` and `external exits`.

**`internal exits`** are triggered by calling the function `exit/1` and make the current process stop its execution.

**`external exits`** are called with `exit/2` and have to do with multiple processes in the concurrent aspect of erlang.

### internal exits

internal exits are pretty similar to `errors`. to understand when to use one or the other, we need to start looking at the concepts of `actors` and `processes` from far away.

**processes** here can send each other messages. a process can also listen for messages, wait for them. you can also choose what messages to listen to, discard some, ignore others, give up listening after a certain time etc.

these basic concepts let the implementors of Erlang use a special kind of message to communicate exceptions between processes. they act a bit like a *process' last breath*; they're sent right before a process dies and the code it contains stops executing. other processes that were listening for that specific kind of message can then know about the event and do whatever they please with it. this includes logging, restarting the process that died, etc.

while both `erlang:error/1` and `exit/1` can be used in an extremely similar manner, the real difference is in the intent. you can then decide whether what you've got is *simply* an error or a condition worthy of *killing the current process*.

`erlang:error/1` returns a stack trace and `exit/1` doesn't. it's because if you were to have a pretty large stack trace or lots of arguments to the current function, copying the exit message to every listening process would mean copying the data. In some cases, this could become unpractical.

### external exits
...

## throws
a throw is a class of exceptions used for cases that the programmer can be expected to handle.

in comparison with `exits` and `errors`, they don't really carry any *crash that process!* intent behind them, but rather control flow.

The syntax to throw an exception is:

    throw(permission_denied).
    -> ** exception throw: permission_denied

throws can also be used for non-local returns when in deep recursion.

example could be the array module, where there is a lookup function that can return a user-supplied default value if it can't find the element needed. when the element can't be found, the value `default` is thrown as an exception, and the top-level function handles that and substitutes it with the user-supplied default value. this keeps the programmer of the module from needing to pass the default value as a parameter of every function of the lookup algorithm. this lets the implementer only write for the successful cases.

</details>
<details>
  <summary><strong>dealing with exceptions</strong></summary><br>

a `try ... catch` is a way to evaluate an expression while letting you handle the successful case as well as the errors encountered.

    try Expression1,...,ExpressionN of
      Pattern1 [when Guard1] -> PatternExpressions1;
      Pattern2 [when Guard2] -> PatternExpressions2;
      ...
      PatternN [when GuardN] -> PatternExpressionN
    catch
      ExceptionType:Reason1 [when ExceptionGuard1] -> ExceptionExpressions1;
      ExceptionType:Reason2 [when ExceptionGuard2] -> ExceptionExpressions2;
      ...
      ExceptionType:ReasonN [when ExceptionGuardN] -> ExceptionExpressionsN
    after
      AfterExpressions
    end.

- the `Expression` in between `try` and `of` is said to be `protected`.
- the `patterns` and `expressions` in between the `try ... of` and `catch` behave in exactly the same manner as a `case ... of`.
- the `catch` part you can replace `TypeOfError` by either `error`, `throw` or `exit`. if no type is provided, a `throw` is assumed.

> **see [exceptions.erl](./code/exceptions/exceptions.erl)**

    exceptions:catch_throws(fun() -> cat end).
    -> ok

    exceptions:try_get(fun() -> cat end).
    -> cat

    exceptions:try_get_2(fun() -> one end).
    -> 1
    exceptions:try_get_2(fun() -> two end).
    -> 2
    exceptions:try_get_2(fun() -> three end).
    -> three

    exceptions:catch_throws(fun() -> throw(thrown) end).
    -> {throw,caught,thrown}

    exceptions:catch_throws(fun() -> erlang:error(pang) end).
    -> ** exception error: pang

we see exception because this `try ... catch` used in `catch_throws` function is only receiving `throws`. as stated earlier, this is because when no type is mentioned, a throw is assumed.

    exceptions:catch_errors(fun() -> erlang:error("Die!") end).
    -> {error,caught,"Die!"}

    exceptions:catch_exits(fun() -> exit(goodbye) end).
    -> {exit,caught,goodbye}

and all in one example;

    exceptions:talk().
    -> "blah blah"

    exceptions:black_knight(fun exceptions:talk/0).
    -> "None shall pass."

    exceptions:black_knight(fun() -> exceptions:sword(1) end).
    -> "It is but a scratch."

    exceptions:black_knight(fun() -> exceptions:sword(2) end).
    -> "I've had worse."

    exceptions:black_knight(fun() -> exceptions:sword(3) end).
    -> "Come on you pansy!"

    exceptions:black_knight(fun() -> exceptions:sword(4) end).
    -> "Just a flesh wound."

    exceptions:black_knight(fun() -> exceptions:sword(5) end).
    -> "Just a flesh wound."

in practice, you should be careful when using the `catch-all` patterns: **try to protect your code from what you can handle, but not any more than that. Erlang has other facilities in place to take care of the rest.**

there's also an additional clause that can be added after a `try ... catch` that will always be executed. this is equivalent to the `finally` block in many other languages: `after`.

    try Expr of
      Pattern -> Expr1
    catch
      Type:Exception -> Expr2
    after % this always gets executed
      Expr3
    end

however, you can not get any return value out of the `after` construct. therefore, `after` is mostly used to run code with **side effects**. the canonical use of this is when you want to make sure a file you were reading gets closed whether exceptions are raised or not.

it is possible to have **more than one expression** between the `try` and the `of`. but when we use many expressions in that manner, we might not always care about what the return value is. the `of` part thus becomes a bit useless. good news is you can just give it up. check `exceptions:whoa/0` and `exceptions:im_impressed/0` functions.

    exceptions:whoa().
    -> {caught,throw,up}

    exceptions:im_impressed().
    -> {caught,throw,up}

</details>
<details>
  <summary><strong>error vs exit vs throw</strong></summary><br>

the real difference between the three types is the communication intent, not a special behaviour. so from the pure theoretical point of view an error exception can be replaced by a throw exception without any side effect. obviously, the communication intent is not negligible: indeed, throw exceptions are usually documented while errors are not intended for being formalized.

**`error`**<br>
error signals that something very bad happened in the system, something that was unexpected to the author of the code raising the exception. even if this type of exception can be raised explicitly, it is usually raised by the Erlang run-time system. this type of exception also contains a stack trace.

**`exit`**<br>
exit means that your code is being told to stop immediately.

**`throw`**<br>
throw identifies an exception that a called function voluntarily raises (throwing it at you); such exceptions shall be documented, i.e. the documentation of the function you are calling shall state that this exception may be raised and specify under what conditions this may happen.
</details>
<details>
  <summary><strong>protected part cant be tail recursive</strong></summary><br>

the protected part of an exception can't be tail recursive. the VM must always keep a reference there in case there's an exception popping up.

because the `try ... catch` construct without the `of` part has nothing but a protected part, calling a recursive function from there might be dangerous for programs supposed to run for a long time (which is Erlang's niche). after enough iterations, you'll go out of memory or your program will get slower without really knowing why. by putting your recursive calls between the `of` and `catch`, you are not in a protected part and you will benefit from `Last Call Optimisation`.

some people use `try ... of ... catch` rather than `try ... catch` by default to avoid unexpected errors of that kind, except for obviously non-recursive code with results that won't be used by anything.
</details>
<details>
  <summary><strong>catch</strong></summary><br>

> **see [exceptions.erl](./code/exceptions/exceptions.erl)**

the keyword `catch` and basically captures all types of exceptions on top of the good results. it displays a different representation of exceptions.

    catch 1+1.
    -> 2

    catch throw(whoa).
    -> whoa

    catch exit(die).
    -> {'EXIT',die}

    catch 1/0.
    -> {'EXIT',{badarith,[{erlang,'/',[1,0],[]},
        {erl_eval,do_apply,6,
                  [{file,"erl_eval.erl"},
                  {line,684}]},
        {erl_eval,expr,5,
                  [{file,"erl_eval.erl"},
                  {line,437}]},
        {shell,exprs,7,
              [{file,"shell.erl"},{line,686}]},
        {shell,eval_exprs,7,
              [{file,"shell.erl"},{line,642}]},
        {shell,eval_loop,3,
              [{file,"shell.erl"},
                {line,627}]}]}}

we can see that `exits` and `errors` are both represented as `{'EXIT', Reason}`. that's due to errors being bolted to the language after exits (they kept a similar representation for backwards compatibility).

the way to read this stack trace is as follows:

    catch doesnt:exist(a,4).
    -> {'EXIT',{undef,[{doesnt,exist,[a,4]},
        {erl_eval,do_apply,5},
        {erl_eval,expr,5},
        {shell,exprs,6},
        {shell,eval_exprs,6},
        {shell,eval_loop,3}]}}

- the type of error is `undef`, which means the function you called is not defined
- the list right after the type of error is a `stack trace`
- the tuple on top of the stack trace represents the last function to be called (`{Module, Function, Arguments}`). That's your undefined function.
- the tuples after that are the functions called before the error. this time they're of the form `{Module, Function, Arity}`.

you can also manually get a stack trace by calling `erlang:get_stacktrace/0` in the process that crashed.

you'll often see catch written in the following manner

    catcher(X,Y) ->
      case catch X/Y of
        {'EXIT', {badarith,_}} -> "uh oh";
        N -> N
      end.

and as expected:

    exceptions:catcher(3,3).
    -> 1.0

    exceptions:catcher(6,0).
    -> "uh oh"

there are a few problems with `catch`

first of it is operator precedence

    X = catch 4+2.
    -> * 1: syntax error before: 'catch'

    X = (catch 4+2).
    -> 6

another problem is that you can't see the difference between `what looks like the underlying representation of an exception` and `a real exception`

    catch erlang:boat().
    -> {'EXIT',{undef,[{erlang,boat,[]},
        {erl_eval,do_apply,5},
        {erl_eval,expr,5},
        {shell,exprs,6},
        {shell,eval_exprs,6},
        {shell,eval_loop,3}]}}

    catch exit({undef,[{erlang,boat,[]},
        {erl_eval,do_apply,5},
        {erl_eval,expr,5},
        {shell,exprs,6},
        {shell,eval_exprs,6},
        {shell,eval_loop,3}]}).
    -> {'EXIT',{undef,[{erlang,boat,[]},
        {erl_eval,do_apply,5},
        {erl_eval,expr,5},
        {shell,exprs,6},
        {shell,eval_exprs,6},
        {shell,eval_loop,3}]}}

and you can't know the difference between `an error` and `an actual exit`. you could also have used `throw/1` to generate the above exception.

in fact, a `throw/1` in a `catch` might also be problematic in another scenario:

    one_or_two(1) -> return;
    one_or_two(2) -> throw(return).

    catch exceptions:one_or_two(1).
    -> return

    catch exceptions:one_or_two(2).
    -> return

because we're behind a catch, we can never know if the function threw an exception or if it returned an actual value!
</details>

## files
<details>
  <summary><strong>reading files</strong></summary><br>

    {ok, Binary} = file:read_file("code/examples/road.txt").
    -> {ok,<<"50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0">>}

    S = string:tokens(binary_to_list(Binary), "\r\n\t ").
    -> ["50","10","30","5","90","20","40","2","25","10","8","0"]

</details>

***

# Definitions
<details>
  <summary><strong>referential transparency</strong></summary><br>

an expression is called referentially transparent if it can be replaced with its corresponding value without changing the program's behavior. This requires that the expression be pure, that is to say the expression value must be the same for the same inputs and its evaluation must have no side effects. An expression that is not referentially transparent is called referentially opaque.
</details>
<details>
  <summary><strong>arity</strong></summary><br>

the arity of a function is an integer representing how many arguments can be passed to the function.
</details>
<details>
  <summary><strong>code hot-loading</strong></summary><br>

upgrading an application while it runs, without stopping it
</details>

<details>
  <summary><strong>hard, firm, and soft real-time</strong></summary><br>

## hard real-time
hard real-time definition considers any missed deadline to be a system failure. This scheduling is used extensively in mission critical systems where failure to conform to timing constraints results in a loss of life or property.

**Examples:**
- Air France Flight 447 crashed into the ocean after a sensor malfunction caused a series of system errors. The pilots stalled the aircraft while responding to outdated instrument readings. All 12 crew and 216 passengers were killed.
- Mars Pathfinder spacecraft was nearly lost when a priority inversion caused system restarts. A higher priority task was not completed on time due to being blocked by a lower priority task. The problem was corrected and the spacecraft landed successfully.
- An Inkjet printer has a print head with control software for depositing the correct amount of ink onto a specific part of the paper. If a deadline is missed then the print job is ruined.

## firm real-time
firm real-time definition allows for infrequently missed deadlines. In these applications the system can survive task failures so long as they are adequately spaced, however the value of the task's completion drops to zero or becomes impossible.

**Examples:**
- Manufacturing systems with robot assembly lines where missing a deadline results in improperly assembling a part. As long as ruined parts are infrequent enough to be caught by quality control and not too costly, then production continues.
- A digital cable set-top box decodes time stamps for when frames must appear on the screen. Since the frames are time order sensitive a missed deadline causes jitter, diminishing quality of service. If the missed frame later becomes available it will only cause more jitter to display it, so it's useless. The viewer can still enjoy the program if jitter doesn't occur too often.

## soft real-time
soft real-time definition allows for frequently missed deadlines, and as long as tasks are timely executed their results continue to have value. Completed tasks may have increasing value up to the deadline and decreasing value past it.

**Examples:**
- Weather stations have many sensors for reading temperature, humidity, wind speed, etc. The readings should be taken and transmitted at regular intervals, however the sensors are not synchronized. Even though a sensor reading may be early or late compared with the others it can still be relevant as long as it is close enough.
- A video game console runs software for a game engine. There are many resources that must be shared between its tasks. At the same time tasks need to be completed according to the schedule for the game to play correctly. As long as tasks are being completely relatively on time the game will be enjoyable, and if not it may only lag a little.

[resource](https://stackoverflow.com/a/30498130)
</details>

<details>
  <summary><strong>statically/dynamically strongly/weakly typed</strong></summary><br>

*there is no universally accepted definition of what these terms mean*

**Static/Dynamic Typing is about when type information is acquired**
- a language is statically-typed if the type of a variable is known at compile-time instead of at run-time.
- a language is dynamically-typed if the type of a variable is checked during run-time.

**Strong/Weak Typing is about how strictly types are distinguished** whether the language tries to do an implicit conversion from strings to numbers.
- a strongly-typed language is one in which variables are bound to specific data types, and will result in type errors if types do not match up as expected in the expression — regardless of when type checking occurs.
- a weakly-typed language on the other hand is a language in which variables are not bound to a specific data type; they still have a type, but type safety constraints are lower compared to strongly-typed languages.

![languages by type system chart](./assets/type_dynamic_static_strong_weak.png)
[resource](https://android.jlelse.eu/magic-lies-here-statically-typed-vs-dynamically-typed-languages-d151c7f95e2b)
</details>
<details>
  <summary><strong>recursions and recursive functions</strong></summary><br>

`recursion` is a way of programming or coding a problem, in which a function calls itself one or more times in its body. usually, it is returning the return value of this function call. if a function definition fulfils the condition of recursion, we call this function a `recursive` function.

**termination/stopping condition**:
a recursive function has to terminate to be used in a program. a recursive function terminates, if with every recursive call the solution of the problem is downsized and moves towards a `base case`. a base case is a case, where the problem can be solved without further recursion. a recursion can lead to an infinite loop, if the base case is not met in the calls.

[resource](https://www.python-course.eu/recursive_functions.php)
</details>
<details>
  <summary><strong>scope and closure</strong></summary><br>

  **scope** defines what variables you have access to.

  whenever you create a function within another function, you have created a **closure**. the inner function is the closure. this closure is usually returned so you can use the outer function’s variables at a later time.
</details>
<details>
  <summary><strong>variable shadowing</strong></summary><br>

**shadowing** is the term used to describe the act of defining a new variable that has the same name as one that was in the parent scope.

    Fn =
      fun() -> A = 1,
        fun(A) -> A = 2 end
      end.

    (Fn())(2).
    -> 2

</details>

***

# Resources

## Learning Sources by Order
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/)

## Tutorials / Presentations
- [Parque - Designing a Real Time Game Engine in Erlang](https://www.youtube.com/watch?v=sla-t0ZNlMU), [[source code](https://github.com/mrallen1/parque)]

## Links
- [Official Docs](http://erlang.org/doc/index.html)
- [Erldocs](https://erldocs.com/)
- [Erlang Resources](https://gist.github.com/macintux/6349828)
- [Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
- [Why did Alan Kay dislike Java](https://www.quora.com/Why-did-Alan-Kay-dislike-Java)

***

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
