-module(hpap).

% APIs
-export([start_link/2, cast/2, balancer_name/1, worker_name/2]).



%% ===================================================================
%% APIs
%% ===================================================================

-callback handle_job(Args :: term()) -> ok.

start_link(PoolName, PoolSize) when is_atom(PoolName), is_integer(PoolSize) ->
    {ok, Pid} = hpap_worker_sup:start_link(PoolName, PoolSize),
    ok = initialize_worker(PoolName, PoolSize),
    {ok, Pid}.


cast(PoolName, Job) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed(A, B, C),
    Key = random:uniform(PoolSize),
    WorkerName = worker_name(PoolName, Key),
    ok = gen_server:cast(WorkerName, {job, Job}),
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

initialize_worker(PoolName, Index) when Index > 0 ->
    WorkerName = hpap:worker_name(PoolName, Index),
    {ok, _} = supervisor:start_child(PoolName,
                                     #{id       => WorkerName,
                                       start    => {hpap_worker, start_link, [PoolName, WorkerName]},
                                       restart  => permanent,
                                       shutdown => brutal_kill,
                                       type     => worker,
                                       modules  => [hpap_worker]}),
    initialize_worker(PoolName, Index - 1);
initialize_worker(_, 0) ->
    ok.