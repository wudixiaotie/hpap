-module(hpap_worker).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, Index) ->
    WorkerName = hpap:worker_name(PoolName, Index),
    gen_server:start_link({local, WorkerName}, ?MODULE, [PoolName], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName]) ->
    {ok, #state{pool_name = PoolName}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.


handle_cast({job, Job}, #state{pool_name = PoolName} = State) ->
    ok = PoolName:handle_job(Job),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================