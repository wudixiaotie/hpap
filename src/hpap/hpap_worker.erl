-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balancer_name}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, WorkerName) ->
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName]) ->
    BalancerName = hpap:balancer_name(PoolName),
    {ok, #state{pool_name = PoolName, balancer_name = BalancerName}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.


handle_cast({job, Job}, #state{pool_name = PoolName, balancer_name = BalancerName} = State) ->
    ok = PoolName:handle_job(Job),
    ok = load_balance(BalancerName),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

load_balance(BalancerName) ->
    {_, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL > 200 of
        true ->io:format("=============migrate~n"),
            ok = migrate(BalancerName, 20),
            ok;
        _ ->
            ok
    end.


migrate(BalancerName, N) when N > 0 ->
    receive
        {job, Job} ->
            BalancerName ! {job, Job},
            migrate(BalancerName, N - 1)
    after
        0 ->
            migrate(BalancerName, 0)
    end;
migrate(_, 0) ->
    ok.