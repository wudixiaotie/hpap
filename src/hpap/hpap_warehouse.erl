-module(hpap_warehouse).

% APIs
-export([start_link/1, init/2]).

% system message
-export ([system_continue/3, system_terminate/4, system_get_state/1,
          system_replace_state/2]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName) ->
    proc_lib:start_link(?MODULE, init, [self(), PoolName]).


init(Parent, PoolName) ->
    SelfPid = self(),
    ets:insert(PoolName, {warehouse_pid, SelfPid}),

    Debug = sys:debug_options([]),
    ok = proc_lib:init_ack(Parent, {ok, SelfPid}),
    loop(Parent, Debug).


system_continue(Parent, Debug, _) ->
    loop(Parent, Debug).


system_terminate(Reason, _Parent, _Debug, _) ->
    exit(Reason).


system_get_state([]) ->
    {ok, [], []}.


system_replace_state(StateFun, []) ->
    NewState = StateFun([]),
    {ok, NewState, NewState}.



%% ===================================================================
%% Internal functions
%% ===================================================================

loop(Parent, Debug) ->
    receive
        {pull, Index, WorkerPid} ->
            ok = send_msg_2_worker(Index, WorkerPid),
            loop(Parent, Debug);
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, [])
    end.


send_msg_2_worker(Index, WorkerPid) ->
    receive
        {Index, Message} ->
            WorkerPid ! Message,
            send_msg_2_worker(Index, WorkerPid)
    after
        0 ->
            ok
    end.