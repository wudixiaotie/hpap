-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balance_threshold}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold, WorkerName) ->
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName, BalanceThreshold], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, BalanceThreshold]) ->
    State = #state{pool_name = PoolName,
                   balance_threshold = BalanceThreshold},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, #state{pool_name = PoolName} = State) ->
    ok = PoolName:handle_task(Task),
    ok = migrate_task(PoolName, State#state.balance_threshold),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

migrate_task(PoolName, BalanceThreshold) ->
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL > BalanceThreshold of
        true ->
            WorkerList = supervisor:which_children(PoolName),
            {ok, Average, MQLList} = average(WorkerList, BalanceThreshold, PoolName),
            ok = do_migrate(MQLList, Average);
        _ ->
            ok
    end.


average(WorkerList, BalanceThreshold, PoolName) ->
    average(WorkerList, BalanceThreshold, PoolName, 0, 0, []).


average([{_, Pid, _, _}|T], BalanceThreshold, PoolName, Sum, Count, MQLList) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    average(T, BalanceThreshold, PoolName, Sum + MQL, Count + 1, [{Pid, MQL}|MQLList]);
average([], BalanceThreshold, PoolName, Sum, Count, MQLList) ->
    MinimunCount = erlang:trunc(Sum / BalanceThreshold) + 1,
    {ok, WorkerCount, WorkerMQLList} = add_new_worker(PoolName, MinimunCount, Count, MQLList),
    Average = erlang:trunc(Sum / WorkerCount) + 1,
    {ok, Average, WorkerMQLList}.


add_new_worker(_, WorkerCount, WorkerCount, WorkerMQLList) ->
    {ok, WorkerCount, WorkerMQLList};
add_new_worker(PoolName, MinimunCount, Count, MQLList) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    NewPoolSize = PoolSize + 1,
    WorkerName = hpap:worker_name(PoolName, NewPoolSize),
    {ok, WorkerPid} = supervisor:start_child(PoolName, [WorkerName]),
    ets:insert(PoolName, {pool_size, NewPoolSize}),
    add_new_worker(PoolName, MinimunCount, Count + 1, [{WorkerPid, 0}|MQLList]).


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