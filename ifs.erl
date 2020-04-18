-module(ifs).
-compile(export_all).

is_two(N) ->
  if
    N =:= 2 -> yes;
    true -> no
  end.

% print stuff
%% this one would be better as a pattern match in function heads!
print_me(Gender, Age, Name) ->
  io:format("Hello ~s!", [Name]),
  if
    Gender =:= male -> io:format(" You are a boy!");
    Gender =:= female -> io:format(" You are a girl!")
  end,
  if
    Age > 18 -> io:format(" You are NOT a teenager!");
    Age =< 18 -> io:format(" You are a teenager!")
  end.

%% this one would be better as a pattern match in function heads!
animal_says(Animal) ->
  Talk =
  if
    Animal == cat  -> "meow";
    Animal == beef -> "mooo";
    Animal == dog  -> "bark";
    Animal == tree -> "bark";
    true -> "idunnowhattosay"
  end,
  {Animal, "says " ++ Talk ++ "!"}.