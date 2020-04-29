-module(multiproc).
-compile([export_all]).

sleep(T) ->
  receive
    after T -> ok
  end.

flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.

send_lots_of_messages_to_self() ->
  self() ! {15, high},
  self() ! {13, high},
  self() ! {7, low},
  self() ! {1, low},
  self() ! {9, low},
  self() ! {17, high},
  self() ! {19, high},
  self() ! {2, low}.

importants_first() ->
  importants(fun normal/0).

importants() ->
  importants(none).

importants(After) ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | importants(After)]
  after 0 ->
    case is_function(After) of
      true -> After();
      false -> []
    end
  end.

normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
  after 0 ->
    []
  end.

optimized(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, hello},
  receive
    {Pid, Ref, Msg} ->
      io:format("~p~n", [Msg])
  end.