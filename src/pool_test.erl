-module(pool_test).

-behaviour(hpap).

% APIs
-export([start_link/0, create/1, handle_task/1]).

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
    timer:sleep(3000),
    ok.


%% ===================================================================
%% Tests
%% ===================================================================

send_msg(Pid, Times) when Times > 0 ->
    Pid ! {task, <<"asdfaSDFAaksjdhfoaiwjef;alskdjflsdf">>},
    % pool_test:create(<<"asdf">>),
    send_msg(Pid, Times - 1);
send_msg(_, 0) ->
    ok.

call_create(Times) when Times > 0 ->
    pool_test:create(<<"asdfaSDFAaksjdhfoaiwjef;alskdjflsdf">>),
    call_create(Times - 1);
call_create(0) ->
    ok.

workers_info() ->
    ChildrenList = supervisor:which_children(?MODULE),
    {hpap_worker_sup, Pid, _, _} = lists:keyfind(hpap_worker_sup, 1, ChildrenList),
    WorkerList = supervisor:which_children(Pid),
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


