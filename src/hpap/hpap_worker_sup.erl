-module(hpap_worker_sup).

-behaviour(supervisor).

% APIs
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Args), #{id => hpap_balancer,
                       start => {hpap_balancer, start_link, Args},
                       restart => permanent,
                       shutdown => brutal_kill,
                       type => worker,
                       modules => [hpap_balancer]}).



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
    {ok, { {one_for_one, 5, 10}, [?CHILD([PoolName, BalanceThreshold])] } }.