-module(hof).
-compile(export_all).

one() -> 1.
two() -> 2.

% hof:add(fun hof:one/0, fun hof:two/0).
add(X, Y) -> X() + Y().

% hof:map(fun hof:decr/1, [1,2,3,4,5]).
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

a() ->
  Secret = "pony",
  fun() -> Secret end.

b(F) ->
  "a/0's password is "++F().