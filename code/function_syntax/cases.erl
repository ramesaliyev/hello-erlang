-module(cases).
-compile(export_all).


% sets are collections of unique values
% this is possibly the worst implementation possible in terms of efficiency, but what we want here is the syntax:
insert(X,[]) -> [X];
insert(X,Set) ->
  case lists:member(X,Set) of
    true  -> Set;
    false -> [X|Set]
  end.

beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 -> 'favorable';
    {kelvin, N} when N >= 293, N =< 318 -> 'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 -> 'favorable in the US';
    _ -> 'avoid beach'
  end.