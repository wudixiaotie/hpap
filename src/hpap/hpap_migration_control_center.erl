-module(hpap_migration_control_center).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balance_threshold}).

-define(TIMEOUT, 600000).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold) ->
    gen_server:start_link(?MODULE, [PoolName, BalanceThreshold], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, BalanceThreshold]) ->
    erlang:process_flag(trap_exit, true),
    ets:insert(PoolName, {migration_control_center_pid, self()}),
    State = #state{pool_name = PoolName, balance_threshold = BalanceThreshold},
    {ok, State, ?TIMEOUT}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, #state{pool_name = PoolName} = State) ->
    self() ! {task, Task},

    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
    WorkerList = supervisor:which_children(WorkerSupPid),

    {ok, NewWorkerCount, Average} = average(WorkerList, State#state.balance_threshold),

    ok = add_new_worker(NewWorkerCount, PoolName),

    NewWorkerList = supervisor:which_children(WorkerSupPid),
    ok = do_migrate(NewWorkerList, Average),
    {noreply, State, ?TIMEOUT};
handle_info(timeout, #state{pool_name = PoolName} = State) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    case PoolSize > 2 of
        true ->
            ets:insert(PoolName, {pool_size, PoolSize - 1}),
            WorkerName = hpap:worker_name(PoolName, PoolSize),
            % tell worker to clean up remaining messages
            WorkerName ! done;
        _ ->
            ok
    end,
    {noreply, State, ?TIMEOUT};
handle_info({done, WorkerPid}, #state{pool_name = PoolName} = State) ->
    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
    % terminate the worker
    ok = supervisor:terminate_child(WorkerSupPid, WorkerPid),
    {noreply, State, ?TIMEOUT};
handle_info(_Info, State) -> {noreply, State, ?TIMEOUT}.


terminate(normal, _State) -> ok;
terminate(shutdown, _State) -> ok;
terminate({shutdown, _Reason}, _State) -> ok;
terminate(_Reason, _State) ->
    % hack: save msg queue to ets
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



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
    WorkerName = hpap:worker_name(PoolName, NewPoolSize),

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
            ok = hpap:send_task(N, Pid);
        _ ->
            ok
    end,
    do_migrate(T, Average);
do_migrate([], _) ->
    ok.