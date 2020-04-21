-module(recursive).
-export([
  fac/1,
  tail_fac/1,
  len/1,
  tail_len/1,
  duplicate/2,
  tail_duplicate/2,
  reverse/1,
  tail_reverse/1,
  sublist/2,
  tail_sublist/2,
  zip/2,
  lenient_zip/2,
  tail_zip/2,
  tail_lenient_zip/2
]).

% factorial with recursion
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

% factorial with tail recursion
tail_fac(N) -> tail_fac(N,1).
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).

% get length of list with recursion
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% get length of list with tail recursion
tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc + 1).

% create a list which length is N and consist of given term with recursion
duplicate(0,_) -> [];
duplicate(N,Term) when N > 0 -> [Term|duplicate(N-1,Term)].

% create a list which length is N and consist of given term with tail recursion
tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(N, Term, List) when N > 0 -> tail_duplicate(N-1, Term, [Term|List]).

% reverse given list with recursion
reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

% reverse given list with tail recursion
tail_reverse(L) -> tail_reverse(L, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T,[H|Acc]).

% get first N element of the list with recursion
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) when N > 0 -> [H|sublist(T,N-1)].

% get first N element of the list with tail recursion
tail_sublist(L, N) -> tail_sublist(L, N, []).
tail_sublist(_, 0, SubList) -> SubList;
tail_sublist([], _, SubList) -> SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
  tail_sublist(T, N-1, SubList ++ [H]).

% zip given lists togehter with recursion
zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs, Ys)].

% zip given lists togehter with recursion and finish whenever one of the two list is done
lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X,Y} | lenient_zip(Xs, Ys)].

% zip given lists togehter with tail recursion
tail_zip(X, Y) -> tail_zip(X, Y, []).
tail_zip([], [], ZipList) -> ZipList;
tail_zip([X|Xs], [Y|Ys], ZipList) -> tail_zip(Xs, Ys, ZipList ++ [{X,Y}]).

% zip given lists togehter with tail recursion and finish whenever one of the two list is done
tail_lenient_zip(X, Y) -> tail_lenient_zip(X, Y, []).
tail_lenient_zip(_, [], ZipList) -> ZipList;
tail_lenient_zip([], _, ZipList) -> ZipList;
tail_lenient_zip([X|Xs], [Y|Ys], ZipList) -> tail_lenient_zip(Xs, Ys, ZipList ++ [{X,Y}]).
