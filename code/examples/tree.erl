-module(tree).
-export([
  empty/0,
  insert/3,
  lookup/2
]).

empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
  {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
  undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
  {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
  lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
  lookup(Key, Larger).


% T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()).
% T2 = tree:insert("Mark Anderson", "i.am.a@hotmail.com", T1).
% Addresses = tree:insert("Anita Bath", "abath@someuni.edu",
%             tree:insert("Kevin Robert", "myfairy@yahoo.com",
%             tree:insert("Wilson Longbrow", "longwil@gmail.com", T2))).
% tree:lookup("Anita Bath", Addresses).