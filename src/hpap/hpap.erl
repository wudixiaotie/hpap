-module(hpap).

% APIs
-export([start_link/2, start_link/3, create/2]).

-define(BALANCE_THRESHLOD, 200).



%% ===================================================================
%% APIs
%% ===================================================================

-callback handle_task(Args :: term()) -> ok.

start_link(PoolName, PoolSize) when is_atom(PoolName), is_integer(PoolSize) ->
    hpap_initialize(PoolName, PoolSize, ?BALANCE_THRESHLOD).


start_link(PoolName, PoolSize, BalanceThreshold) when is_atom(PoolName), is_integer(PoolSize) ->
    hpap_initialize(PoolName, PoolSize, BalanceThreshold).


create(PoolName, Task) ->
    PoolSize = ets:lookup_element(PoolName, pool_size, 2),
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed(A, B, C),
    Key = random:uniform(PoolSize),
    WorkerName = hpap_utility:worker_name(PoolName, Key),
    WorkerName ! {task, Task},
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

hpap_initialize(PoolName, PoolSize, BalanceThreshold) ->
    {ok, Pid} = hpap_sup:start_link(PoolName, PoolSize, BalanceThreshold),
    WorkerSupPid = ets:lookup_element(PoolName, worker_sup_pid, 2),
    ok = initialize_worker(PoolName, WorkerSupPid, PoolSize),
    {ok, Pid}.


initialize_worker(PoolName, WorkerSupPid, Index) when Index > 0 ->
    {ok, _} = supervisor:start_child(WorkerSupPid, [Index]),
    initialize_worker(PoolName, WorkerSupPid, Index - 1);
initialize_worker(_, _, 0) ->
    ok.