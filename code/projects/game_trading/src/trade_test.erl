-module(trade_test).
-compile(export_all).

sleep(Duration) ->
  io:format("---~n"),
  timer:sleep(Duration).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state
test_carl_jim() ->
    PidMain = self(),
    io:format("Main is alive: ~p~n", [PidMain]),
    PidCarlClient = spawn(fun() -> init_client_carl(PidMain) end),
    receive PidCarlFSM -> PidCarlFSM end,
    spawn(fun() -> init_client_jim(PidCarlFSM, PidCarlClient) end),
    init_phase_ok_main_ab.

init_client_carl(PidMain) ->
    % 1
    {ok, PidCarlFSM} = trade_fsm:start_link("Carl"),
    PidMain ! PidCarlFSM,
    io:format("Spawned Carl: ~p~n", [PidCarlFSM]),
    % sys:trace(PidCarlFSM,true),
    sleep(800),

    % 4
    trade_fsm:accept_trade(PidCarlFSM),
    sleep(400),

    % 6
    io:format("~p~n",[trade_fsm:ready(PidCarlFSM)]),
    sleep(1000),

    % 8
    trade_fsm:make_offer(PidCarlFSM, "horse"),
    trade_fsm:make_offer(PidCarlFSM, "sword"),
    sleep(1000),

    % 10
    io:format("a synchronizing~n"),
    sync2(),

    % 12
    trade_fsm:ready(PidCarlFSM),
    sleep(200),
    trade_fsm:ready(PidCarlFSM),
    sleep(1000).

init_client_jim(PidCarlFSM, PidCarlClient) ->
    % 2
    {ok, PidJimFSM} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [PidJimFSM]),
    % sys:trace(PidJimFSM,true),
    sleep(500),

    % 3
    trade_fsm:trade(PidJimFSM, PidCarlFSM),
    trade_fsm:make_offer(PidJimFSM, "boots"),
    sleep(200),

    % 5
    trade_fsm:retract_offer(PidJimFSM, "boots"),
    sleep(500),

    % 7
    trade_fsm:make_offer(PidJimFSM, "shotgun"),
    sleep(1000),

    % 9
    io:format("b synchronizing~n"),
    sync1(PidCarlClient),

    % 11
    trade_fsm:make_offer(PidJimFSM, "horse"), %% race condition!
    trade_fsm:ready(PidJimFSM),
    sleep(200),
    sleep(1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% force a race condition on cd trade negotiation
main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> c(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> d(S, PidC, PidCliC) end),
    receive PidD -> PidD end,
    PidCliC ! PidD.

c(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Marc"),
    Parent ! Pid,
    receive PidD -> PidD end,
    io:format("Spawned Marc: ~p~n", [Pid]),
    %sys:trace(Pid, true),
    sync2(),
    trade_fsm:trade(Pid, PidD),
    %% no need to accept_trade thanks to the race condition
    sleep(200),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "horse"),
    sleep(600),
    trade_fsm:cancel(Pid),
    sleep(1000).

d(Parent, PidC, PidCliC) ->
    {ok, Pid} = trade_fsm:start_link("Pete"),
    Parent ! Pid,
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sync1(PidCliC),
    trade_fsm:trade(Pid, PidC),
    %% no need to accept_trade thanks to the race condition
    sleep(200),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "manatee"),
    sleep(100),
    trade_fsm:ready(Pid),
    sleep(1000).

main_ef() ->
    S = self(),
    PidCliE = spawn(fun() -> e(S) end),
    receive PidE -> PidE end,
    spawn(fun() -> f(PidE, PidCliE) end).

e(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sleep(800),
    trade_fsm:accept_trade(Pid),
    sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),
    sleep(1000),
    trade_fsm:make_offer(Pid, "horse"),
    trade_fsm:make_offer(Pid, "sword"),
    sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    trade_fsm:ready(Pid),
    sleep(200),
    trade_fsm:ready(Pid),
    sleep(1000).

f(PidE, PidCliE) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sleep(500),
    trade_fsm:trade(Pid, PidE),
    trade_fsm:make_offer(Pid, "boots"),
    sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),
    sleep(500),
    trade_fsm:make_offer(Pid, "shotgun"),
    sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliE),
    trade_fsm:make_offer(Pid, "horse"),
    sleep(200),
    trade_fsm:ready(Pid),
    sleep(1000).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.