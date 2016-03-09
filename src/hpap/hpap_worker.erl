-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balancer_name, balance_threshold, task_pack_size}).

-include("hpap.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, WorkerName, Opts) ->
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName, Opts], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, Opts]) ->
    BalancerName = hpap:balancer_name(PoolName),
    State = #state{pool_name = PoolName,
                   balancer_name = BalancerName,
                   balance_threshold = Opts#options.balance_threshold,
                   task_pack_size = Opts#options.task_pack_size},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task, Task}, #state{pool_name = PoolName} = State) ->
    ok = PoolName:handle_task(Task),
    ok = load_balance(State#state.balancer_name,
                      State#state.balance_threshold,
                      State#state.task_pack_size),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

load_balance(BalancerName, BalanceThreshold, TaskPackSize) ->
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL > BalanceThreshold of
        true ->
            ok = migrate(BalancerName, TaskPackSize),
            ok;
        _ ->
            ok
    end.


migrate(BalancerName, TaskPackSize) ->
    migrate(BalancerName, TaskPackSize, []).


migrate(BalancerName, N, TaskPack) when N > 0 ->
    receive
        {task, Task} ->
            migrate(BalancerName, N - 1, [Task|TaskPack])
    after
        0 ->
            migrate(BalancerName, 0, TaskPack)
    end;
migrate(BalancerName, 0, TaskPack) ->
    BalancerName ! {task_pack, TaskPack},
    ok.