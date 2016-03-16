-module(hpap_warehouse).

% APIs
-export([start_link/1, init/2]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName) ->
    proc_lib:start_link(?MODULE, init, [self(), PoolName]).


init(Parent, PoolName) ->
    SelfPid = self(),
    ets:insert(PoolName, {warehouse_pid, SelfPid}),
    ok = proc_lib:init_ack(Parent, {ok, SelfPid}),
    loop().



%% ===================================================================
%% Internal functions
%% ===================================================================

loop() ->
    receive
        {pull, Index, WorkerPid} ->
            ok = send_msg_2_worker(Index, WorkerPid),
            loop()
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