-module(hpap_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type), #{id        => Mod,
                                  start     => {Mod, start_link, Args},
                                  restart   => permanent,
                                  shutdown  => brutal_kill,
                                  type      => Type}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(PoolName, PoolSize, BalanceThreshold) ->
    supervisor:start_link({local, PoolName}, ?MODULE, [PoolName, PoolSize, BalanceThreshold]).




%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([PoolName, PoolSize, BalanceThreshold]) ->
    ets:new(PoolName, [named_table, public, {read_concurrency, true}]),
    ets:insert(PoolName, {pool_size, PoolSize}),

    {ok, { {one_for_one, 5, 10},
           [?CHILD(hpap_migration_control_center,
                   [PoolName, BalanceThreshold],
                   worker),
            ?CHILD(hpap_warehouse,
                   [PoolName],
                   worker),
            ?CHILD(hpap_worker_sup,
                   [PoolName, BalanceThreshold],
                   supervisor)]} }.