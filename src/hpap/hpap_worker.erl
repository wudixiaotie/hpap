-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balancer_name, balance_threshold}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, BalanceThreshold, WorkerName) ->
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName, BalanceThreshold], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, BalanceThreshold]) ->
    BalancerName = hpap:balancer_name(PoolName),
    State = #state{pool_name = PoolName,
                   balancer_name = BalancerName,
                   balance_threshold = BalanceThreshold},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, #state{pool_name = PoolName} = State) ->
    ok = PoolName:handle_task(Task),
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    N = MQL - State#state.balance_threshold,
    case N > 0 of
        true ->
            ok = hpap:send_task(N, State#state.balancer_name);
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

% migrate_path(PoolName) ->
%     WorkerList = supervisor:which_children(PoolName),
%     average_message_queue_len(WorkerList)


% average_message_queue_len(WorkerList) ->
%     average_message_queue_len(WorkerList, 0).


% average_message_queue_len([{_WorkerName, Pid, worker, [hpap_worker]}|T], BalanceThreshold) ->