# Hello Erlang!
These are notes and quotes i take about Erlang while<br>
reading the [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) book.

# Topics
<details>
  <summary><strong>gotchas</strong></summary><br>

- erlang has no such thing as a `null` value
- every function needs to return something
- erlang is built on the notion that a failure in one of the components should not affect the whole system
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

## usage

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

# Tutorials / Presentations
- [Parque - Designing a Real Time Game Engine in Erlang](https://www.youtube.com/watch?v=sla-t0ZNlMU), [[source code](https://github.com/mrallen1/parque)]

# Links
- [Erlang Resources](https://gist.github.com/macintux/6349828)
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
