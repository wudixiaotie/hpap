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
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL > State#state.balance_threshold of
        true ->
            ok = migrate_task(PoolName);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

migrate_task(PoolName) ->
    WorkerList = supervisor:which_children(PoolName),
    {ok, Average, MQLList} = average(WorkerList),
    ok = do_migrate(MQLList, Average),
    ok.


average(WorkerList) ->
    average(WorkerList, 0, 0, []).


average([{_, Pid, _, _}|T], Sum, Count, MQLList) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    average(T, Sum + MQL, Count + 1, [{Pid, MQL}|MQLList]);
average([], Sum, Count, MQLList) ->
    Average = erlang:trunc(Sum / Count) + 1,
    {ok, Average, MQLList}.


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