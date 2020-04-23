-module(tree).
-export([
  empty/0,
  insert/3,
  lookup/2,
  has_value/2
]).

%% create empty tree
empty() -> {node, 'nil'}.

%% insert given Key:Value pair into a Tree, or create Root tree if given tree is empty.
insert(Key, Val, {node, 'nil'}) ->
  {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, {Key, Val, Smaller, Larger}}.

%% look for key in a Tree
lookup(_, {node, 'nil'}) ->
  undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
  {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
  lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
  lookup(Key, Larger).

%% looks for a given value 'Val' in the tree.
has_value(Val, Tree) ->
  try has_value1(Val, Tree) of
    false -> false
  catch
    true -> true
  end.

has_value1(_, {node, 'nil'}) ->
  false;
has_value1(Val, {node, {_, Val, _, _}}) ->
  throw(true); % non-local return
has_value1(Val, {node, {_, _, Left, Right}}) ->
  has_value1(Val, Left),
  has_value1(Val, Right).

% usage examples

% T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()).
% T2 = tree:insert("Mark Anderson", "i.am.a@hotmail.com", T1).

% Addresses = tree:insert("Anita Bath", "abath@someuni.edu",
%             tree:insert("Kevin Robert", "myfairy@yahoo.com",
%             tree:insert("Wilson Longbrow", "longwil@gmail.com", T2))).

% tree:lookup("Anita Bath", Addresses).

% tree:has_value("longwil@gmail.com", Addresses).