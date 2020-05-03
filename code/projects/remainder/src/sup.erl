-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

% This is somewhat similar to the 'restarter',
% although this one is a tad more generic.
% It can take any module, as long as it has
% a start_link function. It will restart the
% process it watches indefinitely, unless the
% supervisor itself is terminated with a `shutdown`
% exit signal.

% there is more advanced and flexible supervisors
% in the chapter about OTP supervisors.
% Those are the ones people are thinking of when
% they mention supervision trees. The supervisor
% demonstrated here is only the most basic form that
% exists and is not exactly fit for production
% environments compared to the real thing.

start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args}).

loop({M, F, A}) ->
  Pid = apply(M, F, A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown); % will kill the child too
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
      loop({M, F, A})
  end.