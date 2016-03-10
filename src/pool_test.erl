-module(pool_test).

-behaviour(hpap).

% APIs
-export([start_link/0, create/1, handle_task/1]).

-export([send_msg/2, workers_info/0]).

-compile (export_all).

%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    hpap:start_link(?MODULE, 10).


create(Task) ->
    hpap:create(?MODULE, Task).


handle_task(Task) ->
    io:format("~p=============some real task: ~p!~n", [self(), Task]),
    timer:sleep(100),
    ok.


send_msg(Pid, Times) when Times > 0 ->
    Pid ! {task, <<"asdfaSDFAaksjdhfoaiwjef;alskdjflsdf">>},
    % pool_test:create(<<"asdf">>),
    send_msg(Pid, Times - 1);
send_msg(_, 0) ->
    ok.

workers_info() ->
    WorkerList = supervisor:which_children(?MODULE),
    workers_info(WorkerList, []).


workers_info([{WorkerName, Pid, worker, [hpap_worker]}|T], Result) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    workers_info(T, [{WorkerName, MQL}|Result]);
workers_info([_|T], Result) ->
    workers_info(T, Result);
workers_info([], Result) ->
    {ok, Result}.


test() ->
    pool_test:send_msg(pool_test_1, 598),
    pool_test:workers_info().
