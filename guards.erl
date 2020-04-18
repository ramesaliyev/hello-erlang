-module(guards).
-compile(export_all).

% old enough to drive?
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

% in right age range to drive?
right_age(X) when X >= 16, X =< 104 -> true;
right_age(_) -> false.

% in wrong age range to drive?
wrong_age(X) when X < 16; X > 104 -> true;
wrong_age(_) -> false.

% return true if b is square of a
is_square(A,B) when B =:= A*A -> true;
is_square(_,_) -> false.

% A or B has to be true, and C has to be true
is_okay(A, B, C) when (A orelse B) andalso C -> true;
is_okay(_, _, _) -> false.