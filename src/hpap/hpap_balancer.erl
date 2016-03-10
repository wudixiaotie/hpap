-module(hpap_balancer).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balance_threshold}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold) ->
    Name = hpap:balancer_name(PoolName),
    gen_server:start_link({local, Name}, ?MODULE, [PoolName, BalanceThreshold], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, BalanceThreshold]) ->
    State = #state{pool_name = PoolName,
                   balance_threshold = BalanceThreshold},
    {ok, State}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, State) ->
    self() ! {task, Task},
    ok = migrate(State#state.pool_name,
                 State#state.balance_threshold),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

migrate_path(PoolName) ->
    WorkerList = supervisor:which_children(PoolName),

migrate(PoolName, BalanceThreshold) ->
    WorkerList = supervisor:which_children(PoolName),
    do_migrate(WorkerList, BalanceThreshold).


do_migrate([{_WorkerName, Pid, worker, [hpap_worker]}|T], BalanceThreshold) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    case MQL > BalanceThreshold of
        true ->
            ok;
        _ ->
            N = BalanceThreshold - MQL,
            ok = hpap:send_task(N, Pid)
    end,
    do_migrate(T, BalanceThreshold);
do_migrate([_|T], BalanceThreshold) ->
    do_migrate(T, BalanceThreshold);
do_migrate([], _) ->
    ok.