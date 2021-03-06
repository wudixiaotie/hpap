-module(hpap_migration_control_center).

-behaviour(gen_msg).

% APIs
-export([start_link/2]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-record(state, {pool_name, balance_threshold}).

-define(TIMEOUT, 600000).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold) ->
    gen_msg:start_link(?MODULE, [PoolName, BalanceThreshold], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([PoolName, BalanceThreshold]) ->
    ets:insert(PoolName, {migration_control_center_pid, self()}),

    % pull messages from warehouse
    WarehousePid = ets:lookup_element(PoolName, warehouse_pid, 2),
    WarehousePid ! {pull, 0, self()},

    State = #state{pool_name = PoolName, balance_threshold = BalanceThreshold},
    {ok, State, ?TIMEOUT}.


handle_msg({task, Task}, #state{pool_name = PoolName} = State) ->
    self() ! {task, Task},

    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
    WorkerList = supervisor:which_children(WorkerSupPid),

    {ok, NewWorkerCount, Average} = average(WorkerList, State#state.balance_threshold),

    ok = add_new_worker(NewWorkerCount, PoolName),

    NewWorkerList = supervisor:which_children(WorkerSupPid),
    ok = do_migrate(NewWorkerList, Average),
    {ok, State, ?TIMEOUT};
handle_msg(timeout, #state{pool_name = PoolName} = State) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    case PoolSize > 2 of
        true ->
            ets:insert(PoolName, {pool_size, PoolSize - 1}),
            WorkerName = hpap_utility:worker_name(PoolName, PoolSize),
            % unregister worker name and tell worker to clean up remaining messages
            WorkerName ! done,
            erlang:unregister(WorkerName);
        _ ->
            ok
    end,
    {ok, State, ?TIMEOUT};
handle_msg({done, WorkerPid}, #state{pool_name = PoolName} = State) ->
    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
    % terminate the worker
    ok = supervisor:terminate_child(WorkerSupPid, WorkerPid),
    {ok, State, ?TIMEOUT};
handle_msg(_Info, State) -> {ok, State, ?TIMEOUT}.


terminate(normal, _State) -> ok;
terminate(shutdown, _State) -> ok;
terminate({shutdown, _Reason}, _State) -> ok;
terminate(_Reason, State) ->
    % send msg to warehouse
    WarehousePid = ets:lookup_element(State#state.pool_name, warehouse_pid, 2),
    ok = hpap_utility:save_messages(WarehousePid, 0),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

average(WorkerList, BalanceThreshold) ->
    average(WorkerList, BalanceThreshold, 0, 0).


average([{_, Pid, _, _}|T], BalanceThreshold, Sum, CurrentWorkerCount) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    average(T, BalanceThreshold, Sum + MQL, CurrentWorkerCount + 1);
average([], BalanceThreshold, Sum, CurrentWorkerCount) ->
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    NewSum = Sum + MQL,
    MinimunWorkerCount = erlang:trunc(NewSum / BalanceThreshold) + 1,

    NewWorkerCount = case MinimunWorkerCount > CurrentWorkerCount of
        true ->
            MinimunWorkerCount - CurrentWorkerCount;
        _ ->
            0
    end,
    Average = erlang:trunc(NewSum / (CurrentWorkerCount + NewWorkerCount)) + 1,
    {ok, NewWorkerCount, Average}.



add_new_worker(NewWorkerCount, PoolName) when NewWorkerCount > 0 ->
    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),

    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    NewPoolSize = PoolSize + 1,
    WorkerName = hpap_utility:worker_name(PoolName, NewPoolSize),

    {ok, _} = supervisor:start_child(WorkerSupPid, [WorkerName]),
    ets:insert(PoolName, {pool_size, NewPoolSize}),
    add_new_worker(NewWorkerCount - 1, PoolName);
add_new_worker(0, _) ->
    ok.


do_migrate([{_, Pid, _, _}|T], Average) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    N = Average - MQL,
    case N > 0 of
        true ->
            ok = hpap_utility:send_task(N, Pid);
        _ ->
            ok
    end,
    do_migrate(T, Average);
do_migrate([], _) ->
    ok.