-module(anonymous).
-compile(export_all).

a() ->
  Secret = "pony",
  fun() -> Secret end.

b(F) ->
  "a/0's password is "++F().

% anonymous:b(anonymous:a()).

shadowed() ->
  A = 1,
  (fun(A) -> A = 2 end)(2).

% when compiling compiler will output;
% Warning: variable 'A' shadowed in 'fun'