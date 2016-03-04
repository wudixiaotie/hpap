-module(pool_test).

-behaviour(hpap).

% APIs
-export([start_link/0, cast/1, handle_job/1]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    hpap:start_link(?MODULE, 10).


cast(Job) ->
    hpap:cast(?MODULE, Job).


handle_job(Job) ->
    io:format("~p=============some real job: ~p!~n", [self(), Job]),
    ok.