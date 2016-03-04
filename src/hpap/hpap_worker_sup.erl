-module(hpap_worker_sup).

-behaviour(supervisor).

% APIs
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id        => hpap_worker,
                 start     => {hpap_worker, start_link, []},
                 restart   => permanent,
                 shutdown  => brutal_kill,
                 type      => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(PoolName, PoolSize) ->
    supervisor:start_link({local, PoolName}, ?MODULE, [PoolName, PoolSize]).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([PoolName, PoolSize]) ->
    ets:new(PoolName, [named_table, public, {read_concurrency, true}]),
    ets:insert(PoolName, {pool_size, PoolSize}),
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD] } }.