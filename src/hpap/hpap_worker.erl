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
            {ok, MQLList, Average, NewWorkerCount} = average(WorkerList, BalanceThreshold, PoolName),
            ok = send_migration_plan(PoolName, MQLList, Average, NewWorkerCount);
        _ ->
            ok
    end.


average(WorkerList, BalanceThreshold, PoolName) ->
    average(WorkerList, BalanceThreshold, PoolName, 0, []).


average([{_, Pid, _, _}|T], BalanceThreshold, PoolName, Sum, MQLList) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    average(T, BalanceThreshold, PoolName, Sum + MQL, [{Pid, MQL}|MQLList]);
average([], BalanceThreshold, PoolName, Sum, MQLList) ->
    CurrentWorkerCount = erlang:length(MQLList),
    MinimunWorkerCount = erlang:trunc(Sum / BalanceThreshold) + 1,
    NewWorkerCount = case MinimunWorkerCount > CurrentWorkerCount of
        true ->
            MinimunWorkerCount - CurrentWorkerCount;
        _ ->
            0
    end,
    Average = erlang:trunc(Sum / (CurrentWorkerCount + NewWorkerCount)) + 1,
    {ok, MQLList, Average, NewWorkerCount}.


send_migration_plan(PoolName, MQLList, Average, NewWorkerCount) ->
    MigrationControlCenterPid = ets:lookup_element(PoolName, migration_control_center_pid, 2),
    MigrationControlCenterPid ! {plan, PoolName, MQLList, Average, NewWorkerCount},
    ok.