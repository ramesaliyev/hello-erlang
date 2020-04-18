-module(pattern).
-compile(export_all).

% greet the person
greet(male, Name) ->
  io:format("Hello, Mr. ~s!~n", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!~n", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!~n", [Name]).

% take a list as an argument and returns its first element
head([H|_]) -> H.

% get the second element of a list
second([_,X|_]) -> X.

% compare if two parameters passed to a function are the same
same(X,X) -> true;
same(_,_) -> false.

% prints a date, but only if it is formatted correctly
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").