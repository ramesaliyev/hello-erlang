-module(fold).
-compile(export_all).

%% map
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

%% filter

%% only keep even numbers
even(L) -> lists:reverse(even(L,[])).
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> even(T, [H|Acc]);
even([_|T], Acc) -> even(T, Acc).

%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).
old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc)
  when Age > 60 -> old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
  old_men(People, Acc).

% both functions operate on lists and have the same objective
% of keeping elements that succeed some test (also a predicate)
% and then drop the others. from this generalization we can
% extract all the useful information we need and abstract them away:

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.

%% fold

%% find the maximum of a list
max([H|T]) -> max2(T, H).
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).

%% find the minimum of a list
min([H|T]) -> min2(T,H).
min2([], Min) -> Min;
min2([H|T], Min) when H < Min -> min2(T,H);
min2([_|T], Min) -> min2(T, Min).

%% sum of all the elements of a list
sum(L) -> sum(L,0).
sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, H+Sum).

% da fold
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

% other functions with fold

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