-module(event).
-compile(export_all).

-record(state, {server,
  name="",
  to_go=0}).

%% the reason why we bind the variable 'Server' in the
%% function head is because it's used in pattern matching
%% in the receive section. Remember, records are hacks!
%% The expression S#state.server is secretly expanded to
%% element(2, S), which isn't a valid pattern to match on.
%% This still works fine for S#state.to_go after the after part,
%% because that one can be an expression left to be evaluated later.

%% Loop uses a list for times in order to go around the ~49 days limit
%% on timeouts.
loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
    if
      Next =:= [] ->
        Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go=Next})
    end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo =
    calendar:datetime_to_gregorian_seconds(TimeOut) -
    calendar:datetime_to_gregorian_seconds(Now),
  Secs =
    if
      ToGo > 0 -> ToGo;
      ToGo =< 0 -> 0
    end,
  normalize(Secs);
time_to_go(Secs) ->
  normalize(Secs).

start(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
  spawn_link(?MODULE, init, [self(), EventName, DateTime]).

%%% Event's innards
init(Server, EventName, DateTime) ->
  loop(#state{
    server = Server,
    name = EventName,
    to_go = time_to_go(DateTime)
  }).

cancel(Pid) ->
  %% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.