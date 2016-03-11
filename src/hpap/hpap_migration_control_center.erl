-module(hpap_migration_control_center).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName) ->
    gen_server:start_link(?MODULE, [PoolName], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName]) ->
    ets:insert(PoolName, {migration_control_center_pid, self()}),
    {ok, #state{pool_name = PoolName}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({plan, PoolName, MQLList, Average, NewWorkerCount}, #state{pool_name = PoolName} = State) ->
    {ok, NewMQLList} = add_new_worker(NewWorkerCount, PoolName, MQLList),
    ok = do_migrate()
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

add_new_worker(NewWorkerCount, PoolName, MQLList) when NewWorkerCount > 0 ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    NewPoolSize = PoolSize + 1,
    WorkerName = hpap:worker_name(PoolName, NewPoolSize),
    {ok, WorkerPid} = supervisor:start_child(PoolName, [WorkerName]),
    ets:insert(PoolName, {pool_size, NewPoolSize}),
    add_new_worker(NewWorkerCount - 1, PoolName, [{WorkerPid, 0}|MQLList]);
add_new_worker(0, _, MQLList) ->
    {ok, MQLList}.


do_migrate([{Pid, MQL}|T], Average) ->
    N = Average - MQL,
    case N > 0 of
        true ->
            ok = send_task(N, Pid);
        _ ->
            ok
    end,
    do_migrate(T, Average);
do_migrate([], _) ->
    ok.


send_task(N, Pid) when N > 0 ->
    receive
        {task, Task} ->
            Pid ! {task, Task},
            send_task(N - 1, Pid);
        _ ->
            send_task(N, Pid)
    after
        0 ->
            send_task(0, Pid)
    end;
send_task(0, _) ->
    ok.