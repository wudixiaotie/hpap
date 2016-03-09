-module(pool_test).

-behaviour(hpap).

% APIs
-export([start_link/0, create/1, handle_task/1]).

-export([send_msg/2]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    hpap:start_link(?MODULE, 10).


create(Task) ->
    hpap:create(?MODULE, Task).


handle_task(Task) ->
    io:format("~p=============some real task: ~p!~n", [self(), Task]),
    981723 rem 233,
    ok.


send_msg(Pid, Times) when Times > 0 ->
    Pid ! {task, <<"asdfaSDFAaksjdhfoaiwjef;alskdjflsdf">>},
    send_msg(Pid, Times - 1);
send_msg(_, 0) ->
    ok.