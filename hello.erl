-module(hello).
-export([hello/0]).
-export([add/2]).
-export([greet_and_add_two/1]).
-export([print_macroex/0]).

-define(sub(X,Y), X-Y).

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
%% format function could be imported with -import(io, [format/1]). and used as format('...')
hello() ->
  io:format("Hello, world!~n").

add(A, B)
  -> A + B.

greet_and_add_two(X) ->
  hello(),
  add(X, 2).

print_macroex() ->
  ?sub(17,10).