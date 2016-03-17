-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balance_threshold, index}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold, Index) ->
    WorkerName = hpap_utility:worker_name(PoolName, Index),
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName, BalanceThreshold, Index], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, BalanceThreshold, Index]) ->
    % pull messages from warehouse
    WarehousePid = ets:lookup_element(PoolName, warehouse_pid, 2),
    WarehousePid ! {pull, Index, self()},

    State = #state{pool_name = PoolName,
                   balance_threshold = BalanceThreshold,
                   index = Index},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, #state{pool_name = PoolName} = State) ->
    ok = PoolName:handle_task(Task),
    ok = migrate_task(PoolName, State#state.balance_threshold),
    {noreply, State};
handle_info(done, State) ->
    MigrationControlCenterPid =
        ets:lookup_element(State#state.pool_name, migration_control_center_pid, 2),

    % tell hpap_migration_controll_center this worker can be terminated
    MigrationControlCenterPid ! {done, self()},
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(normal, _State) -> ok;
terminate(shutdown, _State) -> ok;
terminate({shutdown, _Reason}, _State) -> ok;
terminate(_Reason, State) ->
    % send msg to warehouse
    WarehousePid = ets:lookup_element(State#state.pool_name, warehouse_pid, 2),
    ok = hpap_utility:save_messages(WarehousePid, State#state.index),
    ok.


code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

migrate_task(PoolName, BalanceThreshold) ->
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL > BalanceThreshold of
        true ->
            WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
            WorkerList = supervisor:which_children(WorkerSupPid),
            {ok, Average} = average(WorkerList, BalanceThreshold),

            MigrationControlCenterPid = ets:lookup_element(PoolName, migration_control_center_pid, 2),
            ok = hpap_utility:send_task(MQL - Average, MigrationControlCenterPid);
        _ ->
            ok
    end.


average(WorkerList, BalanceThreshold) ->
    average(WorkerList, BalanceThreshold, 0, 0).


average([{_, Pid, _, _}|T], BalanceThreshold, Sum, CurrentWorkerCount) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    average(T, BalanceThreshold, Sum + MQL, CurrentWorkerCount + 1);
average([], BalanceThreshold, Sum, CurrentWorkerCount) ->
    MinimunWorkerCount = erlang:trunc(Sum / BalanceThreshold) + 1,
    Average = case MinimunWorkerCount > CurrentWorkerCount of
        true ->
            erlang:trunc(Sum / MinimunWorkerCount) + 1;
        _ ->
            erlang:trunc(Sum / CurrentWorkerCount) + 1
    end,
    {ok, Average}.