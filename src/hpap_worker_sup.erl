-module(hpap_worker_sup).

-behaviour(supervisor).

% APIs
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Type), #{id        => Mod,
                            start     => {Mod, start_link, []},
                            restart   => permanent,
                            shutdown  => brutal_kill,
                            type      => Type}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(PoolName) ->
    supervisor:start_link({local, PoolName}, ?MODULE, [PoolName]).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([PoolName]) ->
    ets:new(PoolName, [named_table,
                       public,
                       {read_concurrency, true}]),
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(hpap_worker, [])] } }.