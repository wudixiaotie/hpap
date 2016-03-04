-module(hpap).

% APIs
-export([start_link/2, cast/2]).



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
    case ets:lookup(PoolName, Key) of
        [{_, WorkerPid}] ->
            ok = gen_server:cast(WorkerPid, {job, Job}),
            ok;
        _ ->
            error
    end,
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

initialize_worker(PoolName, Index) when Index > 0 ->
    supervisor:start_child(PoolName, [PoolName, Index]),
    initialize_worker(PoolName, Index - 1);
initialize_worker(_, 0) ->
    ok.