-module(hpap).

% APIs
-export([start_link/2, start_link/3, create/2, balancer_name/1, worker_name/2]).

-include("hpap.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

-callback handle_task(Args :: term()) -> ok.

start_link(PoolName, PoolSize) when is_atom(PoolName), is_integer(PoolSize) ->
    Opts = #options{},
    {ok, Pid} = hpap_worker_sup:start_link(PoolName, Opts, PoolSize),
    ok = initialize_worker(PoolName, Opts, PoolSize),
    {ok, Pid}.


start_link(PoolName, PoolSize, Opts) when is_atom(PoolName), is_integer(PoolSize) ->
    {ok, Pid} = hpap_worker_sup:start_link(PoolName, Opts, PoolSize),
    ok = initialize_worker(PoolName, Opts, PoolSize),
    {ok, Pid}.


create(PoolName, Task) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed(A, B, C),
    Key = random:uniform(PoolSize),
    WorkerName = worker_name(PoolName, Key),
    WorkerName ! {task, Task},
    ok.


balancer_name(PoolName) ->
    PoolNameStr = erlang:atom_to_list(PoolName),
    erlang:list_to_atom(PoolNameStr ++ "_balancer").


worker_name(PoolName, Index) ->
    PoolNameStr = erlang:atom_to_list(PoolName),
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom(PoolNameStr ++ "_" ++ IndexStr).



%% ===================================================================
%% Internal functions
%% ===================================================================

initialize_worker(PoolName, Opts, Index) when Index > 0 ->
    WorkerName = hpap:worker_name(PoolName, Index),
    {ok, _} = supervisor:start_child(PoolName,
                                     #{id       => WorkerName,
                                       start    => {hpap_worker, start_link, [PoolName, WorkerName, Opts]},
                                       restart  => permanent,
                                       shutdown => brutal_kill,
                                       type     => worker,
                                       modules  => [hpap_worker]}),
    initialize_worker(PoolName, Opts, Index - 1);
initialize_worker(_, _, 0) ->
    ok.