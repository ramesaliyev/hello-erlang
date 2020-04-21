-module(quicksort).
-export([quicksort/1, lc_quicksort/1, bestest_qsort/1]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot,Rest,[],[]),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
  if
    H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
    H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.

% there is one one that is simpler and easier to read,
% but makes use of list comprehensions.

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).

% the main differences are that this version is much easier to read,
% but in exchange, it has to traverse the list twice to partition it
% in two parts. This is a fight of clarity against performance,
% but the real loser here is you, because a function lists:sort/1
% already exists. Use that one instead.

% both implementations here need to process values that are equal to
% the pivot more than once. We could have decided to instead
% return 3 lists: elements smaller, larger and equal to the pivot in
% order to make this more efficient.

% another problem relates to how we need to traverse all the partitioned
% lists more than once when attaching them to the pivot. It is possible
% to reduce the overhead a little by doing the concatenation while
% partitioning the lists in three parts.

bestest_qsort([]) -> [];
bestest_qsort(L=[_|_]) ->
  bestest_qsort(L, []).

bestest_qsort([], Acc) -> Acc;
bestest_qsort([Pivot|Rest], Acc) ->
  bestest_partition(Pivot, Rest, {[], [Pivot], []}, Acc).

bestest_partition(_, [], {Smaller, Equal, Larger}, Acc) ->
  bestest_qsort(Smaller, Equal ++ bestest_qsort(Larger, Acc));
bestest_partition(Pivot, [H|T], {Smaller, Equal, Larger}, Acc) ->
  if
    H < Pivot ->
      bestest_partition(Pivot, T, {[H|Smaller], Equal, Larger}, Acc);
    H > Pivot ->
      bestest_partition(Pivot, T, {Smaller, Equal, [H|Larger]}, Acc);
    H == Pivot ->
      bestest_partition(Pivot, T, {Smaller, [H|Equal], Larger}, Acc)
  end.